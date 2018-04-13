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

package forms.register

import forms.mappings.Mappings
import models.person.PersonDetails
import play.api.data.Form
import play.api.data.Forms.{mapping, optional}

class IndividualDetailsFormProvider extends Mappings {

  val nameMaxLength = 35

  def apply(): Form[PersonDetails] = Form(
    mapping(
      "firstName" -> text("messages__error__first_name").verifying(returnOnFirstFailure(
        maxLength(nameMaxLength, "messages__error__first_name_length"),
        regexp(regexName, "messages__error__first_name_invalid"))
      ),
      "middleName" -> optional(text("messages__error__middle_name").verifying(returnOnFirstFailure(
        maxLength(nameMaxLength, "messages__error__middle_name_length"),
        regexp(regexName, "messages__error__middle_name_invalid"))
      )),
      "lastName" -> text("messages__error__last_name").verifying(returnOnFirstFailure(
        maxLength(nameMaxLength, "messages__error__last_name_length"),
        regexp(regexName, "messages__error__last_name_invalid"))
      ),
      "date" -> dateMapping("messages__error__date").verifying(futureDate("messages__error__date_future"))
    )(PersonDetails.apply)(PersonDetails.unapply)
  )

}