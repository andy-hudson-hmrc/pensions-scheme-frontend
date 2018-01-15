/*
 * Copyright 2018 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package forms.mappings

import models.EstablisherNino.{No, Yes}
import models.{EstablisherNino, SchemeType, SortCode}
import models.{SchemeType, SortCode, UniqueTaxReference}
import models.SchemeType.{BodyCorporate, GroupLifeDeath, Other, SingleTrust}
import models.UniqueTaxReference.{No, Yes}
import org.joda.time.LocalDate
import play.api.data.{FieldMapping, FormError, Forms, Mapping}
import play.api.data.Forms.of
import utils.Enumerable
import play.api.data.Forms._
import play.api.data.format.Formatter
import uk.gov.voa.play.form.ConditionalMappings._

import scala.util.Try

trait Mappings extends Formatters with Constraints {

  protected def text(errorKey: String = "error.required"): FieldMapping[String] =
    of(stringFormatter(errorKey))

  protected def int(requiredKey: String = "error.required",
                    wholeNumberKey: String = "error.wholeNumber",
                    nonNumericKey: String = "error.nonNumeric"): FieldMapping[Int] =
    of(intFormatter(requiredKey, wholeNumberKey, nonNumericKey))

  protected def boolean(requiredKey: String = "error.required",
                        invalidKey: String = "error.boolean"): FieldMapping[Boolean] =
    of(booleanFormatter(requiredKey, invalidKey))

  protected def enumerable[A](requiredKey: String = "error.required",
                              invalidKey: String = "error.invalid")(implicit ev: Enumerable[A]): FieldMapping[A] =
    of(enumerableFormatter[A](requiredKey, invalidKey))

  protected def schemeTypeMapping(requiredTypeKey: String = "messages__error__selection",
                                  invalidTypeKey: String = "messages__error__scheme_type_invalid",
                                  requiredOtherKey: String = "messages__error__scheme_type_information",
                                  invalidOtherKey: String = "messages__error__scheme_type_length"): Mapping[SchemeType] = {
    val schemeTypeDetailsMaxLength = 150
    val other = "other"

    def fromSchemeType(schemeType: SchemeType): (String, Option[String]) = {
      schemeType match {
        case SchemeType.Other(someValue) => (other, Some(someValue))
        case _ => (schemeType.toString, None)
      }
    }

    def toSchemeType(schemeTypeTuple: (String, Option[String])): SchemeType = {

      val mappings: Map[String, SchemeType] = Seq(
        SingleTrust,
        GroupLifeDeath,
        BodyCorporate
      ).map(v => (v.toString, v)).toMap

      schemeTypeTuple match {
        case (key, Some(value)) if(key == other) => Other(value)
        case (key, _) if mappings.keySet.contains(key) => {
          mappings.apply(key)
        }
      }
    }

    tuple(
      "type" -> text(requiredTypeKey).verifying(schemeTypeConstraint(invalidTypeKey)),
      "schemeTypeDetails" -> mandatoryIfEqual("schemeType.type", other, text(requiredOtherKey).
        verifying(maxLength(schemeTypeDetailsMaxLength, invalidOtherKey)))
    ).transform(toSchemeType, fromSchemeType)
  }

  protected def uniqueTaxReferenceMapping(requiredKey: String = "messages__error__has_sautr_establisher",
                                          requiredUtrKey: String = "messages__error__sautr",
                                          requiredReasonKey: String = "messages__error__no_sautr_establisher",
                                          invalidUtrKey: String = "messages__error__sautr_invalid",
                                          maxLengthReasonKey: String = "messages__error__no_sautr_length"):
    Mapping[UniqueTaxReference] = {

    val regexUtr = "\\d{10}"
    val reasonMaxLength = 150
    def fromUniqueTaxReference(utr: UniqueTaxReference): (Boolean, Option[String], Option[String]) = {
      utr match {
        case UniqueTaxReference.Yes(utr) => (true, Some(utr), None)
        case UniqueTaxReference.No(reason) =>  (false, None, Some(reason))
      }
    }

    def toUniqueTaxReference(utrTuple: (Boolean, Option[String], Option[String])) = {

      utrTuple match {
        case (true, Some(utr), None) => Yes(utr)
        case (false, None, Some(reason)) => No(reason)
        case _ => throw new RuntimeException("Invalid selection")
      }
    }

    tuple("hasUtr" -> boolean(requiredKey),
    "utr" -> mandatoryIfTrue("uniqueTaxReference.hasUtr", text(requiredUtrKey).verifying(regexp(regexUtr, invalidUtrKey))),
    "reason" -> mandatoryIfFalse("uniqueTaxReference.hasUtr",
      text(requiredReasonKey).verifying(maxLength(reasonMaxLength, maxLengthReasonKey)))).
      transform(toUniqueTaxReference, fromUniqueTaxReference)
  }

  protected def establisherNinoMapping(requiredKey: String = "messages__error__has_nino_establisher",
                                       requiredNinoKey: String = "messages__error__nino",
                                       requiredReasonKey: String = "messages__establisher__no_nino",
                                       invalidNinoKey: String = "messages__error__nino_invalid"):
  Mapping[EstablisherNino] = {

    def fromEstablisherNino(nino: EstablisherNino): (String, Option[String], Option[String]) = {
      nino match {
        case EstablisherNino.Yes(nino) => ("yes", Some(nino), None)
        case EstablisherNino.No(reason) =>  ("no", None, Some(reason))
      }
    }

    def toEstablisherNino(ninoTuple: (String, Option[String], Option[String])) = {

      ninoTuple match {
        case (hasNino, Some(nino), _) if hasNino == "yes" => Yes(nino)
        case (hasNino, _, Some(reason)) if hasNino == "no" => No(reason)
      }
    }

    tuple("hasNino" -> text(requiredKey),
      "nino" -> mandatoryIfEqual("establisherNino.hasNino", "yes",
        text(requiredNinoKey).verifying(validNino(invalidNinoKey))),
      "reason" -> mandatoryIfEqual("establisherNino.hasNino", "no", text(requiredReasonKey))).
      transform(toEstablisherNino, fromEstablisherNino)
  }

  protected def dateMapping(invalidKey: String): Mapping[LocalDate] = {

    def toLocalDate(date: (String, String, String)): LocalDate =
    {
      date match {
        case (day, month, year) =>
          new LocalDate(year.toInt, month.toInt, day.toInt)
      }
    }

    def fromLocalDate(date: LocalDate): (String, String, String) = {
      (date.getDayOfMonth.toString, date.getMonthOfYear.toString, date.getYear.toString)
    }

    def validateDate(date: (String, String, String)): Boolean =
      Try(toLocalDate(date)).isSuccess

    tuple("day" -> text(invalidKey),
    "month" -> text(invalidKey),
    "year" -> text(invalidKey)).verifying(invalidKey, validateDate(_)).transform(toLocalDate, fromLocalDate)
  }

  protected def sortCodeMapping(requiredKey: String = "error.required", invalidKey: String, maxErrorKey: String): Mapping[SortCode] = {

    val formatter: Formatter[SortCode] = new Formatter[SortCode] {

      val baseFormatter = stringFormatter(requiredKey)
      val regexSortCode = """\d*""".r.toString()

      override def bind(key: String, data: Map[String, String]): Either[Seq[FormError], SortCode] = {

        baseFormatter.bind(key, data)
          .right.map(_.trim.replaceAll("[ -]", ""))
          .right.flatMap {
          case str if !str.matches(regexSortCode)  =>
            Left(Seq(FormError(key, invalidKey)))
          case str if str.trim.replaceAll("[- ]", "").length > 6 =>
            Left(Seq(FormError(key, maxErrorKey)))
          case str =>
            val a :: b :: c :: Nil = str.sliding(2, 2).toList
            Right(SortCode(a, b, c))
        }
      }

      override def unbind(key: String, value: SortCode): Map[String, String] =
        baseFormatter.unbind(key, s"${value.first} ${value.second} ${value.third}")
    }

    Forms.of(formatter)
  }
}
