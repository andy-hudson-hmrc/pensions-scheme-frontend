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

package views.register

import controllers.register.routes
import forms.register.SecuredBenefitsFormProvider
import models.NormalMode
import play.api.data.Form
import play.twirl.api.HtmlFormat
import views.behaviours.YesNoViewBehaviours
import views.html.register.securedBenefits

class SecuredBenefitsViewSpec extends YesNoViewBehaviours {

  val messageKeyPrefix = "contractofinsurance_pension_scheme"

  val form = new SecuredBenefitsFormProvider()()

  val schemeName = "myScheme"

  def createView: () => HtmlFormat.Appendable = () => securedBenefits(frontendAppConfig, form, NormalMode, schemeName)(fakeRequest, messages)

  def createViewUsingForm: Form[_] => HtmlFormat.Appendable = (form: Form[_]) =>
    securedBenefits(frontendAppConfig, form, NormalMode, schemeName)(fakeRequest, messages)

  "SecuredBenefits view" must {

    behave like normalPage(createView, messageKeyPrefix, messages(s"messages__${messageKeyPrefix}__title"), "_copy")

    behave like pageWithBackLink(createView)

    behave like yesNoPage(
      createView = createViewUsingForm,
      messageKeyPrefix = messageKeyPrefix,
      expectedFormAction = routes.SecuredBenefitsController.onSubmit(NormalMode).url
    )

    behave like pageWithSecondaryHeader(createView, schemeName)
  }
}
