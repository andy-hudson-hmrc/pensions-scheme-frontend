/*
 * Copyright 2017 HM Revenue & Customs
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

import models.SchemeType
import org.apache.commons.lang3.RandomStringUtils
import org.scalatest.{MustMatchers, OptionValues, WordSpec}
import play.api.data.{Form, FormError}
import utils.Enumerable

object MappingsSpec {

  sealed trait Foo
  case object Bar extends Foo
  case object Baz extends Foo

  object Foo {

    val values: Set[Foo] = Set(Bar, Baz)

    implicit val fooEnumerable: Enumerable[Foo] =
      Enumerable(values.toSeq.map(v => v.toString -> v): _*)
  }
}

class MappingsSpec extends WordSpec with MustMatchers with OptionValues with Mappings {

  import MappingsSpec._

  "text" must {

    val testForm: Form[String] =
      Form(
        "value" -> text()
      )

    "bind a valid string" in {
      val result = testForm.bind(Map("value" -> "foobar"))
      result.get mustEqual "foobar"
    }

    "not bind an empty string" in {
      val result = testForm.bind(Map("value" -> ""))
      result.errors must contain(FormError("value", "error.required"))
    }

    "not bind an empty map" in {
      val result = testForm.bind(Map.empty[String, String])
      result.errors must contain(FormError("value", "error.required"))
    }

    "return a custom error message" in {
      val form = Form("value" -> text("custom.error"))
      val result = form.bind(Map("value" -> ""))
      result.errors must contain(FormError("value", "custom.error"))
    }

    "unbind a valid value" in {
      val result = testForm.fill("foobar")
      result.apply("value").value.value mustEqual "foobar"
    }
  }

  "boolean" must {

    val testForm: Form[Boolean] =
      Form(
        "value" -> boolean()
      )

    "bind true" in {
      val result = testForm.bind(Map("value" -> "true"))
      result.get mustEqual true
    }

    "bind false" in {
      val result = testForm.bind(Map("value" -> "false"))
      result.get mustEqual false
    }

    "not bind a non-boolean" in {
      val result = testForm.bind(Map("value" -> "not a boolean"))
      result.errors must contain(FormError("value", "error.boolean"))
    }

    "not bind an empty value" in {
      val result = testForm.bind(Map("value" -> ""))
      result.errors must contain(FormError("value", "error.required"))
    }

    "not bind an empty map" in {
      val result = testForm.bind(Map.empty[String, String])
      result.errors must contain(FormError("value", "error.required"))
    }

    "unbind" in {
      val result = testForm.fill(true)
      result.apply("value").value.value mustEqual "true"
    }
  }

  "int" must {

    val testForm: Form[Int] =
      Form(
        "value" -> int()
      )

    "bind a valid integer" in {
      val result = testForm.bind(Map("value" -> "1"))
      result.get mustEqual 1
    }

    "not bind an empty value" in {
      val result = testForm.bind(Map("value" -> ""))
      result.errors must contain(FormError("value", "error.required"))
    }

    "not bind an empty map" in {
      val result = testForm.bind(Map.empty[String, String])
      result.errors must contain(FormError("value", "error.required"))
    }

    "unbind a valid value" in {
      val result = testForm.fill(123)
      result.apply("value").value.value mustEqual "123"
    }
  }

  "enumerable" must {

    val testForm = Form(
      "value" -> enumerable[Foo]()
    )

    "bind a valid option" in {
      val result = testForm.bind(Map("value" -> "Bar"))
      result.get mustEqual Bar
    }

    "not bind an invalid option" in {
      val result = testForm.bind(Map("value" -> "Not Bar"))
      result.errors must contain(FormError("value", "error.invalid"))
    }

    "not bind an empty map" in {
      val result = testForm.bind(Map.empty[String, String])
      result.errors must contain(FormError("value", "error.required"))
    }
  }

  "schemeType" must {
    val validSchemeTypeDetailsLength = 150
    val invalidSchemeTypeDetailsLength = 151

    val testForm: Form[SchemeType] = Form(
      "schemeType" -> schemeTypeMapping("schemeType.error.required", "schemeType.error.invalid",
        "messages__error__scheme_type_information", "messages__error__scheme_type_length")
    )

    "bind a valid schemeType SingleTrust" in {
      val result = testForm.bind(Map("schemeType.type" -> "single"))
      result.get mustEqual SchemeType.SingleTrust
    }

    "bind a valid schemeType GroupLifeDeath" in {
      val result = testForm.bind(Map("schemeType.type" -> "group"))
      result.get mustEqual SchemeType.GroupLifeDeath
    }

    "bind a valid schemeType BodyCorporate" in {
      val result = testForm.bind(Map("schemeType.type" -> "corp"))
      result.get mustEqual SchemeType.BodyCorporate
    }

    "bind a valid schemeType Other" in {
      val result = testForm.bind(Map("schemeType.type" -> "other", "schemeType.schemeTypeDetails" -> "some value"))
      result.get mustEqual SchemeType.Other("some value")
    }

    "not bind an empty Map" in {
      val result = testForm.bind(Map.empty[String, String])
      result.errors must contain(FormError("schemeType.type", "schemeType.error.required"))
    }

    "not bind a Map with invalid schemeType" in {
      val result = testForm.bind(Map("schemeType.type" -> "Invalid"))
      result.errors must contain(FormError("schemeType.type", "schemeType.error.invalid"))
    }

    "not bind a Map with type other but no schemeTypeDetails" in {
      val result = testForm.bind(Map("schemeType.type" -> "other"))
      result.errors must contain(FormError("schemeType.schemeTypeDetails", "messages__error__scheme_type_information"))
    }

    "not bind a Map with type other and schemeTypeDetails exceeds max length 150" in {
      val testString = RandomStringUtils.random(invalidSchemeTypeDetailsLength)
      val result = testForm.bind(Map("schemeType.type" -> "other", "schemeType.schemeTypeDetails" -> testString))
      result.errors must contain(FormError("schemeType.schemeTypeDetails", "messages__error__scheme_type_length",
        Seq(validSchemeTypeDetailsLength)))
    }

    "unbind a valid schemeType SingleTrust" in {
      val result = testForm.fill(SchemeType.SingleTrust)
      result.apply("schemeType.type").value.value mustEqual "single"
    }

    "unbind a valid schemeType GroupLifeDeath" in {
      val result = testForm.fill(SchemeType.GroupLifeDeath)
      result.apply("schemeType.type").value.value mustEqual "group"
    }

    "unbind a valid schemeType BodyCorporate" in {
      val result = testForm.fill(SchemeType.BodyCorporate)
      result.apply("schemeType.type").value.value mustEqual "corp"
    }

    "unbind a valid schemeType Other" in {
      val result = testForm.fill(SchemeType.Other("some value"))
      result.apply("schemeType.type").value.value mustEqual "other"
      result.apply("schemeType.schemeTypeDetails").value.value mustEqual "some value"
    }
  }
}
