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

import javax.inject.Inject
import forms.mappings.AddressMapping
import play.api.data.Form

  class PostCodeLookupFormProvider @Inject() extends AddressMapping {

    def apply(): Form[String] =
      Form(
        "value" -> postCodeMapping(
          "messages__error__postcode",
          "messages__error__postcode_length",
          "messages__error__postcode_invalid"
        )
      )
  }