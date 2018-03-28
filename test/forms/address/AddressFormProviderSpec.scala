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

package forms.address

import forms.FormSpec
import forms.behaviours.{AddressBehaviours, FormBehaviours}
import forms.mappings.AddressMapping
import models.address.Address
import utils.FakeCountryOptions

import scala.util.Random

class AddressFormProviderSpec extends FormBehaviours with FormSpec with AddressBehaviours {

  private def alphaString(max: Int = maxAddressLineLength) =
    Random.alphanumeric take Random.shuffle(Range(1, max).toList).head mkString ""

  private val addressLine1 = alphaString()
  private val addressLine2 = alphaString()
  private val addressLine3 = alphaString()
  private val addressLine4 = alphaString()
  private val postCode = "ZZ1 1ZZ"

  private val countryOptions = FakeCountryOptions()

  val validData: Map[String, String] = Map(
    "addressLine1" -> addressLine1,
    "addressLine2" -> addressLine2,
    "addressLine3" -> addressLine3,
    "addressLine4" -> addressLine4,
    "postCode" -> postCode,
    "country" -> "GB"
  )

  val form = new AddressFormProvider(countryOptions)()

  "Address form" must {
    behave like questionForm(Address(
      addressLine1,
      addressLine2,
      Some(addressLine3),
      Some(addressLine4),
      Some(postCode),
      "GB"
    ))

    behave like formWithCountry(
      form,
      "country",
      "messages__error_country_required",
      "messages__error_country_invalid",
      countryOptions,
      Map(
        "addressLine1" -> addressLine1,
        "addressLine2" -> addressLine2
      )
    )

    behave like formWithCountryAndPostCode(
      form,
      "messages__error__postcode",
      "messages__error__postcode_invalid",
      Map(
        "addressLine1" -> addressLine1,
        "addressLine2" -> addressLine2
      ),
      (address: Address) => address.postCode.getOrElse("")
    )

    "behave like a form with address lines" when {

      behave like formWithAddressField(
        form,
        "addressLine1",
        "messages__error__address_line_1_required",
        "messages__error__address_line_1_length",
        "messages__error__address_line_1_invalid"
      )

      behave like formWithAddressField(
        form,
        "addressLine2",
        "messages__error__address_line_2_required",
        "messages__error__address_line_2_length",
        "messages__error__address_line_2_invalid"
      )

      behave like formWithOptionalAddressField(
        form,
        "addressLine3",
        "messages__error__address_line_3_length",
        "messages__error__address_line_3_invalid"
      )

      behave like formWithOptionalAddressField(
        form,
        "addressLine4",
        "messages__error__address_line_4_length",
        "messages__error__address_line_4_invalid"
      )
    }
  }
}