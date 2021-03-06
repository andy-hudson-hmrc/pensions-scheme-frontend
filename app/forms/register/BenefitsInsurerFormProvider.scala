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
import javax.inject.Inject
import models.register.BenefitsInsurer
import play.api.data.Form
import play.api.data.Forms._

class BenefitsInsurerFormProvider @Inject() extends Mappings {
  val maxLength = 160
  val policyMaxLength = 55

  def apply(): Form[BenefitsInsurer] = Form(
    mapping(
      "companyName" -> text("messages__error__company_name").verifying(
        firstError(maxLength(maxLength, "messages__error__company_name_length"),
          safeText("messages__error__company_name_invalid")
        )
      ),
      "policyNumber" -> text("messages__error__benefits_insurance__policy").verifying(
        firstError(maxLength(policyMaxLength, "messages__error__benefits_insurance__policy_length"),
          safeText("messages__error__benefits_insurance__policy_invalid")
        )
      )
    )(BenefitsInsurer.apply)(BenefitsInsurer.unapply)
  )
}
