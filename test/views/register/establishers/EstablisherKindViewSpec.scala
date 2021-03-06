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

package views.register.establishers

import forms.register.establishers.EstablisherKindFormProvider
import models.register.establishers.EstablisherKind
import models.{Index, NormalMode}
import play.api.data.Form
import views.behaviours.ViewBehaviours
import views.html.register.establishers.establisherKind

class EstablisherKindViewSpec extends ViewBehaviours {

  private val messageKeyPrefix = "establishers__add"

  private val form = new EstablisherKindFormProvider()()

  private val schemeName = "test scheme name"

  private def createView = () => establisherKind(frontendAppConfig, form, NormalMode, Index(1), schemeName)(fakeRequest, messages)

  private def createViewUsingForm = (form: Form[_]) => establisherKind(frontendAppConfig, form, NormalMode, Index(1), schemeName)(fakeRequest, messages)

  private def establisherKindOptions = EstablisherKind.options.filter { option =>
    option.value match {
      case EstablisherKind.Partnership.toString => frontendAppConfig.allowPartnerships
      case _ => true
    }
  }

  "EstablisherKind view" must {
    behave like normalPage(createView, messageKeyPrefix, messages("messages__establishers__add__title"))

    behave like pageWithBackLink(createView)
  }

  "EstablisherKind view" when {
    "rendered" must {
      "contain radio buttons for the value" in {
        val doc = asDocument(createViewUsingForm(form))
        for (option <- establisherKindOptions) {
          assertContainsRadioButton(doc, s"value-${option.value}", "value", option.value, isChecked = false)
        }
      }
    }

    for (option <- establisherKindOptions) {
      s"rendered with a value of '${option.value}'" must {
        s"have the '${option.value}' radio button selected" in {
          val doc = asDocument(createViewUsingForm(form.bind(Map("value" -> s"${option.value}"))))
          assertContainsRadioButton(doc, s"value-${option.value}", "value", option.value, isChecked = true)

          for (unselectedOption <- establisherKindOptions.filterNot(o => o == option)) {
            assertContainsRadioButton(doc, s"value-${unselectedOption.value}", "value", unselectedOption.value, isChecked = false)
          }
        }
      }
    }
  }

}
