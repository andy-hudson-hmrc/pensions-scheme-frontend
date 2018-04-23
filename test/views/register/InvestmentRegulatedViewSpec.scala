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

import play.api.data.Form
import controllers.register.routes
import forms.register.InvestmentRegulatedFormProvider
import views.behaviours.YesNoViewBehaviours
import models.NormalMode
import views.html.register.investmentRegulated

class InvestmentRegulatedViewSpec extends YesNoViewBehaviours {

  val messageKeyPrefix = "investment_regulated"

  val form = new InvestmentRegulatedFormProvider()()

  val schemeName = "myScheme"

  def createView = () => investmentRegulated(frontendAppConfig, form, NormalMode, schemeName)(fakeRequest, messages)

  def createViewUsingForm = (form: Form[_]) => investmentRegulated(frontendAppConfig, form, NormalMode, schemeName)(fakeRequest, messages)

  "InvestmentRegulated view" must {

    behave like normalPage(createView, messageKeyPrefix, messages(s"messages__${messageKeyPrefix}__title"))

    behave like pageWithBackLink(createView)

    behave like pageWithSecondaryHeader(createView, schemeName)

    behave like yesNoPage(createView = createViewUsingForm, messageKeyPrefix = messageKeyPrefix,
      expectedFormAction = routes.InvestmentRegulatedController.onSubmit(NormalMode).url)
  }
}
