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

package views.register.trustees

import play.api.data.Form
import controllers.register.trustees.routes
import forms.register.trustees.MoreThanTenTrusteesFormProvider
import views.behaviours.YesNoViewBehaviours
import models.NormalMode
import views.html.register.trustees.moreThanTenTrustees

class MoreThanTenTrusteesViewSpec extends YesNoViewBehaviours {

  val messageKeyPrefix = "moreThanTenTrustees"
  val schemeName = "Test Scheme Name"
  val form = new MoreThanTenTrusteesFormProvider()()

  def createView = () => moreThanTenTrustees(frontendAppConfig, form, NormalMode, schemeName)(fakeRequest, messages)

  def createViewUsingForm = (form: Form[_]) => moreThanTenTrustees(frontendAppConfig, form, NormalMode, schemeName)(fakeRequest, messages)

  "MoreThanTenTrustees view" must {

    behave like normalPage(createView, messageKeyPrefix, messages("messages__moreThanTenTrustees__heading"))

    behave like pageWithBackLink(createView)

    behave like pageWithSecondaryHeader(createView, schemeName)

    behave like yesNoPage(createViewUsingForm, messageKeyPrefix, routes.MoreThanTenTrusteesController.onSubmit(NormalMode).url, expectedHintKey = Some("_hint"))
  }
}