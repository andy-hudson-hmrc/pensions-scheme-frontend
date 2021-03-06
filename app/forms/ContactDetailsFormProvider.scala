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

package forms

import forms.mappings.{EmailMapping, PhoneNumberMapping}
import javax.inject.Inject
import models.ContactDetails
import play.api.data.Form
import play.api.data.Forms._

class ContactDetailsFormProvider @Inject() extends EmailMapping with PhoneNumberMapping {

  def apply(): Form[ContactDetails] = Form(
    mapping(
      "emailAddress" -> emailMapping(
        "messages__error__email",
        "messages__error__email_length",
        "messages__error__email_invalid"
      ),
      "phoneNumber" -> phoneNumberMapping(
        "messages__error__phone",
        "messages__error__phone_length",
        "messages__error__phone_invalid"
      )
    )(ContactDetails.apply)(ContactDetails.unapply)
  )

}
