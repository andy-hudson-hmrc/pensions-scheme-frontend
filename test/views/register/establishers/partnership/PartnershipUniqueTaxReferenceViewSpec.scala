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

package views.register.establishers.partnership

import forms.register.establishers.partnership.PartnershipUniqueTaxReferenceFormProvider
import models.{Index, NormalMode}
import play.api.data.Form
import play.twirl.api.HtmlFormat
import views.behaviours.ViewBehaviours
import views.html.register.establishers.partnership.partnershipUniqueTaxReference

class PartnershipUniqueTaxReferenceViewSpec extends ViewBehaviours {

  val messageKeyPrefix = "partnership_has_utr"

  val form: Form[_] = new PartnershipUniqueTaxReferenceFormProvider().apply()

  val index = Index(1)

  val partnershipName = "test partnership name"

  def createView: () => HtmlFormat.Appendable = () => partnershipUniqueTaxReference(frontendAppConfig,
    form, NormalMode, index, partnershipName)(fakeRequest, messages)

  def createViewUsingForm: Form[_] => HtmlFormat.Appendable = (form: Form[_]) => partnershipUniqueTaxReference(frontendAppConfig, form,
    NormalMode, index, partnershipName)(fakeRequest, messages)

  "PartnershipUniqueTaxReference view" must {

    behave like normalPage(createView, messageKeyPrefix, messages("messages__partnership_has_utr__title"))

    behave like pageWithBackLink(createView)

    behave like pageWithSecondaryHeader(createView, partnershipName)
  }

  "PartnershipUniqueTaxReference view" when {
    "rendered" must {
      val utrOptions = Seq("true", "false")

      "contain radio buttons for the value" in {
        val doc = asDocument(createViewUsingForm(form))
        for (option <- utrOptions) {
          assertContainsRadioButton(doc, s"uniqueTaxReference_hasUtr-$option", "uniqueTaxReference.hasUtr", option, isChecked = false)
        }
      }

      for (option <- utrOptions) {
        s"rendered with a value of '$option'" must {
          s"have the '$option' radio button selected" in {
            val doc = asDocument(createViewUsingForm(form.bind(Map("uniqueTaxReference.hasUtr" -> s"$option"))))
            assertContainsRadioButton(doc, s"uniqueTaxReference_hasUtr-$option", "uniqueTaxReference.hasUtr", option, isChecked = true)

            for (unselectedOption <- utrOptions.filterNot(o => o == option)) {
              assertContainsRadioButton(doc, s"uniqueTaxReference_hasUtr-$unselectedOption", "uniqueTaxReference.hasUtr",
                unselectedOption, isChecked = false)
            }
          }
        }
      }

      "display an input text box with the value when yes is selected" in {
        val expectedValue = "1234567891"
        val doc = asDocument(createViewUsingForm(form.bind(Map("uniqueTaxReference.hasUtr" -> "true", "uniqueTaxReference.utr" ->
          expectedValue))))
        doc must haveLabelAndValue("uniqueTaxReference_utr", s"${messages("messages__common__utr")} ${
          messages(
            "messages__partnership_utr_hint_format")
        }",
          expectedValue)
      }

      "display an input text box with the value when no is selected" in {
        val expectedValue = "don't have ctutr"
        val doc = asDocument(createViewUsingForm(form.bind(Map("uniqueTaxReference.hasUtr" -> "false", "uniqueTaxReference.reason" ->
          expectedValue))))
        doc must haveLabelAndValue("uniqueTaxReference_reason", messages(
          "messages__hint__no_utr_partnership"), expectedValue)
      }
    }
  }
}
