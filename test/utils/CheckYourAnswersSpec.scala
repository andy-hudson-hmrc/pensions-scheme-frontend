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

package utils

import identifiers.TypedIdentifier
import identifiers.register.SchemeEstablishedCountryId
import models._
import models.address.Address
import models.person.PersonDetails
import models.register._
import models.requests.DataRequest
import org.joda.time.LocalDate
import org.scalatest.prop.PropertyChecks
import org.scalatest.{MustMatchers, OptionValues, WordSpec}
import play.api.libs.json._
import play.api.mvc.AnyContent
import play.api.test.FakeRequest
import uk.gov.hmrc.domain.PsaId
import utils.checkyouranswers.Ops._
import viewmodels.AnswerRow

import scala.language.implicitConversions

class CheckYourAnswersSpec extends WordSpec with MustMatchers with PropertyChecks with OptionValues with Enumerable.Implicits {

  val onwardUrl = "onwardUrl"

  def testIdentifier[A]: TypedIdentifier[A] = new TypedIdentifier[A] {
    override def toString = "testId"
  }

  "CheckYourAnswers" must {

    "produce row of answers" when {

      "string" when {

        "id is schemeEstablishedCountryId" in {
          implicit val countryOptions = new CountryOptions(Seq(InputOption("AU", "Australia"),
            InputOption("GB", "United Kingdom")))
          implicit val request: DataRequest[AnyContent] = DataRequest(FakeRequest(), "id", UserAnswers(Json.obj(SchemeEstablishedCountryId.toString -> "AU")), PsaId("A0000000"))

          SchemeEstablishedCountryId.row(onwardUrl) must equal(Seq(
            AnswerRow("schemeEstablishedCountry.checkYourAnswersLabel", Seq("Australia"), false, onwardUrl)))
        }

        "any id other than schemeEstablishedCountryId" in {
          implicit val countryOptions = new CountryOptions(Seq.empty[InputOption])
          implicit val request: DataRequest[AnyContent] = DataRequest(FakeRequest(), "id", UserAnswers(Json.obj("testId" -> "value")), PsaId("A0000000"))

          testIdentifier[String].row(onwardUrl) must equal(Seq(AnswerRow("testId.checkYourAnswersLabel", Seq("value"), false, onwardUrl)))
        }
      }

      "boolean" in {
        implicit val request: DataRequest[AnyContent] = DataRequest(FakeRequest(), "id", UserAnswers(Json.obj("testId" -> true)), PsaId("A0000000"))

        testIdentifier[Boolean].row(onwardUrl) must equal(Seq(AnswerRow("testId.checkYourAnswersLabel", Seq("site.yes"), true, onwardUrl)))
      }

      "schemeDetails" in {
        val schemeType = SchemeType.SingleTrust
        implicit val request: DataRequest[AnyContent] = DataRequest(FakeRequest(), "id", UserAnswers(
          Json.obj("testId" -> SchemeDetails("test name", schemeType))), PsaId("A0000000"))

        testIdentifier[SchemeDetails].row(onwardUrl) must equal(Seq(
          AnswerRow("messages__scheme_details__name_label", Seq("test name"), false, onwardUrl),
          AnswerRow("messages__scheme_details__type_legend_short", Seq(s"messages__scheme_details__type_$schemeType"), true, onwardUrl)
        ))
      }

      "membership" in {
        val membershipVal = Membership.options.head.value
        implicit val request: DataRequest[AnyContent] = DataRequest(FakeRequest(), "id", UserAnswers(Json.obj("testId" -> membershipVal)), PsaId("A0000000"))

        testIdentifier[Membership].row(onwardUrl) must equal(Seq(AnswerRow(
          "testId.checkYourAnswersLabel", Seq(s"messages__membership__$membershipVal"), true, onwardUrl)))
      }

      "bankDetails" in {
        val date = LocalDate.now()
        implicit val request: DataRequest[AnyContent] = DataRequest(FakeRequest(), "id", UserAnswers(
          Json.obj("testId" -> UKBankDetails("test bank name", "test account name", SortCode("12", "23", "45"), "test account number", date))), PsaId("A0000000"))

        testIdentifier[UKBankDetails].row(onwardUrl) must equal(Seq(
          AnswerRow("messages__uk_bank_account_details__bank_name", Seq("test bank name"), false, onwardUrl),
          AnswerRow("messages__uk_bank_account_details__account_name", Seq("test account name"), false, onwardUrl),
          AnswerRow("messages__uk_bank_account_details__sort_code", Seq("12-23-45"), false, onwardUrl),
          AnswerRow("messages__uk_bank_account_details__account_number", Seq("test account number"), false, onwardUrl),
          AnswerRow("bankAccountDate.checkYourAnswersLabel", Seq(s"${DateHelper.formatDate(date)}"), false, onwardUrl)
        ))
      }

      "schemeBenefits" in {
        val benefitsVal = Benefits.options.head.value
        implicit val request: DataRequest[AnyContent] = DataRequest(FakeRequest(), "id", UserAnswers(Json.obj("testId" -> benefitsVal)), PsaId("A0000000"))

        testIdentifier[Benefits].row(onwardUrl) must equal(Seq(AnswerRow(
          "messages__benefits__title", Seq(s"messages__benefits__$benefitsVal"), true, onwardUrl)))
      }

      "benefitsInsurer" in {
        implicit val request: DataRequest[AnyContent] = DataRequest(FakeRequest(), "id", UserAnswers(
          Json.obj("testId" -> BenefitsInsurer("test name", "test policy"))), PsaId("A0000000"))

        testIdentifier[BenefitsInsurer].row(onwardUrl) must equal(Seq(
          AnswerRow("messages__benefits_insurance__name", Seq("test name"), false, onwardUrl),
          AnswerRow("messages__benefits_insurance__policy", Seq("test policy"), false, onwardUrl)
        ))
      }

      "companyDetails" when {

        "only name exists" in {

          val companyDetails = CompanyDetails("Company Name", None, None)

          implicit val request: DataRequest[AnyContent] = DataRequest(FakeRequest(), "id", UserAnswers(Json.obj(
            "testId" -> companyDetails
          )), PsaId("A0000000"))

          testIdentifier[CompanyDetails].row("onwardUrl") must equal(Seq(AnswerRow(
            "messages__common__cya__name",
            Seq(companyDetails.companyName),
            false,
            onwardUrl
          )))

        }
        "vat number exists" in {

          val companyDetails = CompanyDetails("Company Name", Some("VAT123"), None)

          implicit val request: DataRequest[AnyContent] = DataRequest(FakeRequest(), "id", UserAnswers(Json.obj(
            "testId" -> companyDetails
          )), PsaId("A0000000"))

          testIdentifier[CompanyDetails].row(onwardUrl) must equal(Seq(
            AnswerRow(
              "messages__common__cya__name",
              Seq(s"${companyDetails.companyName}"),
              false,
              onwardUrl
            ),
            AnswerRow(
              "messages__common__cya__vat",
              Seq(s"${companyDetails.vatNumber.get}"),
              false,
              onwardUrl
            )))

        }
        "paye ref exists" in {

          val companyDetails = CompanyDetails("Company Name", None, Some("PAYE/123"))

          implicit val request: DataRequest[AnyContent] = DataRequest(FakeRequest(), "id", UserAnswers(Json.obj(
            "testId" -> companyDetails
          )), PsaId("A0000000"))

          testIdentifier[CompanyDetails].row(onwardUrl) must equal(Seq(
            AnswerRow(
              "messages__common__cya__name",
              Seq(s"${companyDetails.companyName}"),
              false,
              onwardUrl
            ),
            AnswerRow(
              "messages__common__cya__paye",
              Seq(s"${companyDetails.payeNumber.get}"),
              false,
              onwardUrl
            )))

        }

        "all values exist" in {

          val companyDetails = CompanyDetails("Company Name", Some("VAT123"), Some("PAYE/123"))

          implicit val request: DataRequest[AnyContent] = DataRequest(FakeRequest(), "id", UserAnswers(Json.obj(
            "testId" -> companyDetails
          )), PsaId("A0000000"))

          testIdentifier[CompanyDetails].row(onwardUrl) must equal(Seq(
            AnswerRow(
              "messages__common__cya__name",
              Seq(s"${companyDetails.companyName}"),
              false,
              onwardUrl
            ),
            AnswerRow(
              "messages__common__cya__vat",
              Seq(s"${companyDetails.vatNumber.get}"),
              false,
              onwardUrl
            ),
            AnswerRow(
              "messages__common__cya__paye",
              Seq(s"${companyDetails.payeNumber.get}"),
              false,
              onwardUrl
            )))

        }
      }

      "CRN" when {

        "yes" in {

          val crn = CompanyRegistrationNumber.Yes("0987654")

          implicit val request: DataRequest[AnyContent] = DataRequest(FakeRequest(), "id", UserAnswers(Json.obj("testId" -> crn)), PsaId("A0000000"))

          testIdentifier[CompanyRegistrationNumber].row(onwardUrl) must equal(Seq(
            AnswerRow(
              "messages__company__cya__crn_yes_no",
              Seq(s"${CompanyRegistrationNumber.Yes}"),
              true,
              onwardUrl
            ),
            AnswerRow(
              "messages__common__crn",
              Seq(s"${crn.crn}"),
              true,
              onwardUrl
            )))
        }
        "no" in {

          val crn = CompanyRegistrationNumber.No("Not sure")

          implicit val request: DataRequest[AnyContent] = DataRequest(FakeRequest(), "id", UserAnswers(Json.obj("testId" -> crn)), PsaId("A0000000"))

          testIdentifier[CompanyRegistrationNumber].row(onwardUrl) must equal(Seq(
            AnswerRow(
              "messages__company__cya__crn_yes_no",
              Seq(s"${CompanyRegistrationNumber.No}"),
              true,
              onwardUrl
            ),
            AnswerRow(
              "messages__company__cya__crn_no_reason",
              Seq(s"${crn.reason}"),
              true,
              onwardUrl
            )
          ))

        }
      }

      "UTR" when {

        "yes" in {

          val utr = UniqueTaxReference.Yes("7654321244")

          implicit val request: DataRequest[AnyContent] = DataRequest(FakeRequest(), "id", UserAnswers(Json.obj("testId" -> utr)), PsaId("A0000000"))

          testIdentifier[UniqueTaxReference].row(onwardUrl) must equal(Seq(
            AnswerRow(
              "messages__establisher_individual_utr_question_cya_label",
              Seq(s"${UniqueTaxReference.Yes}"),
              false,
              onwardUrl
            ),
            AnswerRow(
              "messages__establisher_individual_utr_cya_label",
              Seq({
                utr.utr
              }),
              false,
              onwardUrl
            )
          ))

        }

        "no" in {

          val utr = UniqueTaxReference.No("Not sure")

          implicit val request: DataRequest[AnyContent] = DataRequest(FakeRequest(), "id", UserAnswers(Json.obj("testId" -> utr)), PsaId("A0000000"))

          testIdentifier[UniqueTaxReference].row(onwardUrl) must equal(Seq(
            AnswerRow(
              "messages__establisher_individual_utr_question_cya_label",
              Seq(s"${UniqueTaxReference.No}"),
              false,
              onwardUrl
            ),
            AnswerRow(
              "messages__establisher_individual_utr_reason_cya_label",
              Seq(utr.reason),
              false,
              onwardUrl
            )))

        }

      }

      "address" in {

        implicit val countryOptions = new CountryOptions(Seq.empty[InputOption])

        val address = Address(
          "address1", "address2", Some("address3"), Some("address4"), Some("postcode"), "GB"
        )

        implicit val request: DataRequest[AnyContent] = DataRequest(FakeRequest(), "id", UserAnswers(Json.obj("testId" -> address)), PsaId("A0000000"))

        def addressAnswer(address: Address): Seq[String] = {
          val country = countryOptions.options.find(_.value == address.country).map(_.label).getOrElse(address.country)

          Seq(
            Some(s"${address.addressLine1},"),
            Some(s"${address.addressLine2},"),
            address.addressLine3.map(line3 => s"$line3,"),
            address.addressLine4.map(line4 => s"$line4,"),
            address.postcode.map(postCode => s"$postCode,"),
            Some(country)
          ).flatten
        }

        testIdentifier[Address].row(onwardUrl) must equal(Seq(
          AnswerRow(
            "messages__common__cya__address",
            addressAnswer(address),
            false,
            onwardUrl
          )))

      }

      "address years" in {

        val addressYears = AddressYears.values.head

        implicit val request: DataRequest[AnyContent] = DataRequest(FakeRequest(), "id", UserAnswers(Json.obj("testId" -> addressYears)), PsaId("A0000000"))

        testIdentifier[AddressYears].row(onwardUrl) must equal(Seq(AnswerRow(
          "messages__establisher_address_years__title",
          Seq(s"messages__common__$addressYears"),
          true,
          onwardUrl
        )))

      }

      "contactDetails" in {

        val contactDetails = ContactDetails("e@mail.com", "0987654")

        implicit val request: DataRequest[AnyContent] = DataRequest(FakeRequest(), "id", UserAnswers(Json.obj("testId" -> contactDetails)), PsaId("A0000000"))

        testIdentifier[ContactDetails].row(onwardUrl) must equal(Seq(
          AnswerRow(
            "messages__common__email",
            Seq(s"${contactDetails.emailAddress}"),
            false,
            onwardUrl
          ),
          AnswerRow(
            "messages__common__phone",
            Seq(s"${contactDetails.phoneNumber}"),
            false,
            onwardUrl
          )))

      }

      "personDetails" in {
        val personDetails = PersonDetails("firstName", None, "last", LocalDate.now)
        implicit val request: DataRequest[AnyContent] = DataRequest(FakeRequest(), "id", UserAnswers(Json.obj("testId" -> personDetails)), PsaId("A0000000"))

        testIdentifier[PersonDetails].row(onwardUrl) must equal(Seq(
          AnswerRow(
            "messages__common__cya__name",
            Seq(s"${personDetails.fullName}"),
            false,
            onwardUrl
          ),
          AnswerRow(
            "messages__common__dob",
            Seq(s"${DateHelper.formatDate(personDetails.date)}"),
            false,
            onwardUrl
          )))
      }

      "Nino" when {
        "yes" in {
          val nino = Nino.Yes("AB700100A")
          implicit val request: DataRequest[AnyContent] = DataRequest(FakeRequest(), "id", UserAnswers(Json.obj("testId" -> nino)), PsaId("A0000000"))

          testIdentifier[Nino].row(onwardUrl) must equal(Seq(
            AnswerRow(
              "messages__trusteeNino_question_cya_label",
              Seq(s"${Nino.Yes}"),
              false,
              onwardUrl
            ),
            AnswerRow(
              "messages__trusteeNino_nino_cya_label",
              Seq(nino.nino),
              false,
              onwardUrl
            )))
        }

        "no" in {
          val nino = Nino.No("Not sure")
          implicit val request: DataRequest[AnyContent] = DataRequest(FakeRequest(), "id", UserAnswers(Json.obj("testId" -> nino)), PsaId("A0000000"))

          testIdentifier[Nino].row(onwardUrl) must equal(Seq(
            AnswerRow(
              "messages__trusteeNino_question_cya_label",
              Seq(s"${Nino.No}"),
              false,
              onwardUrl
            ),
            AnswerRow(
              "messages__trusteeNino_reason_cya_label",
              Seq(nino.reason),
              false,
              onwardUrl
            )))
        }
      }
    }
  }
}
