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

package forms.register.establishers.partnership.partner

import forms.mappings.NinoMapping
import javax.inject.Inject
import models.Nino
import play.api.data.Form

class PartnerNinoFormProvider @Inject() extends NinoMapping {

  def apply(): Form[Nino] = Form(
    "nino" -> ninoMapping(
      requiredKey = "messages__error__has_nino_partner",
      requiredReasonKey = "messages__partner_no_nino"
    )
  )
}
