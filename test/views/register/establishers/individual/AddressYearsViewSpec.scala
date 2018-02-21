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

package views.register.establishers.individual

import play.api.data.Form
import forms.register.establishers.individual.AddressYearsFormProvider
import models.register.establishers.individual.AddressYears
import models.{Index, NormalMode}
import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import org.scalatestplus.play.OneAppPerSuite
import play.twirl.api.HtmlFormat
import views.ViewAssertions
import views.behaviours.ViewBehaviours
import views.html.register.establishers.individual.addressYears

class AddressYearsViewSpec extends ViewBehaviours with ViewAssertions with OneAppPerSuite {

  val messageKeyPrefix = "establisher_address_years"

  val form = new AddressYearsFormProvider()()

//  def createView: () => HtmlFormat.Appendable = () => addressYears(frontendAppConfig, form, NormalMode, Index(0))(fakeRequest, messages)
//
//  def createViewUsingForm: Form[_] => HtmlFormat.Appendable = (form: Form[_]) =>
//    addressYears(frontendAppConfig, form, NormalMode, Index(0))(fakeRequest, messages)
//
//  "AddressYears view" must {
//    behave like normalPage(createView, messageKeyPrefix, messages(s"messages__${messageKeyPrefix}__title"))
//
//    behave like pageWithBackLink(createView)
//  }
//
//  "AddressYears view" when {
//    "rendered" must {
//      "contain radio buttons for the value" in {
//        val doc = asDocument(createViewUsingForm(form))
//        for (option <- AddressYears.options) {
//          assertContainsRadioButton(doc, s"value-${option.value}", "value", option.value, false)
//        }
//      }
//    }
//
//    for(option <- AddressYears.options) {
//      s"rendered with a value of '${option.value}'" must {
//        s"have the '${option.value}' radio button selected" in {
//          val doc = asDocument(createViewUsingForm(form.bind(Map("value" -> s"${option.value}"))))
//          assertContainsRadioButton(doc, s"value-${option.value}", "value", option.value, true)
//
//          for(unselectedOption <- AddressYears.options.filterNot(o => o == option)) {
//            assertContainsRadioButton(doc, s"value-${unselectedOption.value}", "value", unselectedOption.value, false)
//          }
//        }
//      }
//    }
//  }

  "AddressYears view" must {

    lazy val page: Document = Jsoup.parse(addressYears(frontendAppConfig, form, NormalMode, Index(0))(fakeRequest, messages).toString)

    "rendered without errors" when {

      "not display the error summary" in {
        page mustNot haveAnErrorSummary
      }

      "have a back link" in {
        page must haveLink(messages("site.back") -> "#")
      }

      "have the correct page title" in {
        page must haveBrowserTitle(messages("messages__establisher_address_years__title"))
      }

      "have the correct heading" in {
        page must haveHeading(messages("messages__establisher_address_years__title"))
      }

      "have a fieldset with the correct legend" in {
        page must haveFieldset(messages("messages__establisher_address_years__title"))
      }

      AddressYears.options.foreach {
        option =>
          s"have a radio button with label: '${messages(option.label)}' and value: '${option.value}'" in {
            page must haveRadioButton(messages(option.label), option.value)
          }
      }

      "have a submit button" in {
        page must haveButton(messages("site.save_and_continue"))
      }
    }

    "rendered with errors" when {

    }
  }
}
