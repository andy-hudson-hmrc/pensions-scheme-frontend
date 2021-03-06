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

package views.register.establishers.company

import controllers.register.establishers.company.routes
import forms.address.PostCodeLookupFormProvider
import models.{Index, NormalMode}
import org.jsoup.Jsoup
import play.api.data.Form
import play.twirl.api.HtmlFormat
import views.behaviours.StringViewBehaviours
import views.html.register.establishers.company.companyPostCodeLookup

class CompanyPostCodeLookupViewSpec extends StringViewBehaviours {

  val messageKeyPrefix = "establisher_company_postcode"

  val form = new PostCodeLookupFormProvider()()
  val firstIndex = Index(0)
  val companyName = "test company name"

  def createView: () => HtmlFormat.Appendable = () => companyPostCodeLookup(frontendAppConfig, form, NormalMode, firstIndex,
    companyName)(fakeRequest, messages)

  def createViewUsingForm: Form[String] => HtmlFormat.Appendable = (form: Form[String]) => companyPostCodeLookup(frontendAppConfig, form,
    NormalMode, firstIndex, companyName)(fakeRequest, messages)

  "Address view" must {
    behave like normalPage(createView, messageKeyPrefix, messages(s"messages__${messageKeyPrefix}__title"))

    behave like pageWithBackLink(createView)

    behave like stringPage(createViewUsingForm, messageKeyPrefix, routes.CompanyPostCodeLookupController.onSubmit(NormalMode, firstIndex).url,
      Some("messages__common__address_postcode"))

    "have company name rendered on the page" in {
      Jsoup.parse(createView().toString()) must haveDynamicText(companyName)
    }

    "have link for enter address manually" in {
      Jsoup.parse(createView().toString()).select("a[id=manual-address-link]") must haveLink(
        routes.CompanyPostCodeLookupController.onPageLoad(NormalMode, firstIndex).url)
    }
  }
}
