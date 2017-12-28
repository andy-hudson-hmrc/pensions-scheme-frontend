/*
 * Copyright 2017 HM Revenue & Customs
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
import forms.register.UKBankDetailsFormProvider
import models.NormalMode
import models.UKBankDetails
import org.apache.commons.lang3.RandomUtils
import org.joda.time.LocalDate
import views.behaviours.QuestionViewBehaviours
import views.html.register.uKBankDetails

class UKBankDetailsViewSpec extends QuestionViewBehaviours[UKBankDetails] {

  val messageKeyPrefix = "uk_bank_account_details"

  override val form = new UKBankDetailsFormProvider()()

  def createView = () => uKBankDetails(frontendAppConfig, form, NormalMode)(fakeRequest, messages)

  def createViewUsingForm = (form: Form[_]) => uKBankDetails(frontendAppConfig, form, NormalMode)(fakeRequest, messages)

  val validData: Map[String, String] = Map(
    "bankName" -> "test bank",
    "accountName" -> "test account",
    "sortCode" -> RandomUtils.nextInt(100000, 999999).toString,
    "accountNumber" -> RandomUtils.nextInt(10000000, 99999999).toString,
    "date.day" -> "1",
    "date.month" -> "2",
    "date.year" -> LocalDate.now().getYear.toString
  )

  "UKBankDetails view" must {

    behave like normalPage(createView, messageKeyPrefix)

    behave like pageWithTextFields(createViewUsingForm, messageKeyPrefix,
      routes.UKBankDetailsController.onSubmit(NormalMode).url, "bankName", "accountName", "sortCode",
      "accountNumber")

    "display an input text box with the correct label and value for day" in {
      val doc = asDocument(createViewUsingForm(form.bind(validData)))
      doc must haveLabelAndValue("date_day", messages("messages__common__day"), "1")
    }

    "display an input text box with the correct label and value for month" in {
      val doc = asDocument(createViewUsingForm(form.bind(validData)))
      doc must haveLabelAndValue("date_month", messages("messages__common__month"), "2")
    }

    "display an input text box with the correct label and value for year" in {
      val doc = asDocument(createViewUsingForm(form.bind(validData)))
      doc must haveLabelAndValue("date_year", messages("messages__common__year"), LocalDate.now().getYear.toString)
    }
  }
}
