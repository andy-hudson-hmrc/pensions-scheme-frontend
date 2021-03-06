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

package forms.register.establishers.individual

import forms.behaviours.NinoBehaviours

class EstablisherNinoFormProviderSpec extends NinoBehaviours {

  val requiredKey = "messages__error__has_nino_establisher"
  val requiredNinoKey = "messages__error__nino"
  val requiredReasonKey = "messages__establisher__no_nino"
  val reasonLengthKey: String = "messages__error__no_nino_length"
  val invalidReasonKey = "messages__error__no_nino_invalid"
  val invalidNinoKey = "messages__error__nino_invalid"

  val testForm = new EstablisherNinoFormProvider().apply()

  "EstablisherNino form provider" must {
    behave like formWithNino(testForm,
      requiredKey,
      requiredNinoKey,
      requiredReasonKey,
      reasonLengthKey,
      invalidNinoKey,
      invalidReasonKey
    )
  }
}
