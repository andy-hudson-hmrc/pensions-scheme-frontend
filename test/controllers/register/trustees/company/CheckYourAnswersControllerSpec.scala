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

package controllers.register.trustees.company

import controllers.ControllerSpecBase
import controllers.actions.{DataRequiredActionImpl, DataRetrievalAction, FakeAuthAction}
import models.{CheckMode, Index}
import play.api.test.Helpers._
import utils.{CheckYourAnswersFactory, CountryOptions, FakeCountryOptions, InputOption}
import viewmodels.{AnswerRow, AnswerSection}
import views.html.check_your_answers

class CheckYourAnswersControllerSpec extends ControllerSpecBase {

  val countryOptions: CountryOptions = new CountryOptions(Seq(InputOption("GB", "United Kingdom")))
  val firstIndex = Index(0)
  val companyName = "test company name"

  val answers: Seq[AnswerRow] = Seq.empty

  lazy val companyDetailsRoute = routes.CompanyDetailsController.onPageLoad(CheckMode, firstIndex).url

  lazy val postUrl = routes.CheckYourAnswersController.onSubmit(firstIndex)

  lazy val companyDetailsSection = AnswerSection(
    Some("messages__checkYourAnswers__section__company_details"),
    Seq(
      AnswerRow("messages__common__cya__name", Seq(companyName), false, companyDetailsRoute),
      AnswerRow("messages__company__cya__vat", Seq("123456"), false, companyDetailsRoute),
      AnswerRow("messages__company__cya__paye_ern", Seq("abcd"), false, companyDetailsRoute)
    )
  )

  lazy val contactDetailsSection = AnswerSection(
    Some("messages__checkYourAnswers__section__contact_details"),
    Seq.empty[AnswerRow]
  )

  def controller(dataRetrievalAction: DataRetrievalAction = getMandatoryEstablisher): CheckYourAnswersController =
    new CheckYourAnswersController(
      frontendAppConfig,
      messagesApi,
      FakeAuthAction,
      dataRetrievalAction,
      new DataRequiredActionImpl,
      new CheckYourAnswersFactory(countryOptions),
      new FakeCountryOptions
    )

  lazy val viewAsString = check_your_answers(
    frontendAppConfig,
    Seq(
      companyDetailsSection,
      contactDetailsSection
    ),
    Some(companyName),
    postUrl
  )(fakeRequest, messages).toString

  "Check Your Answers Controller" must {
    "return 200 and the correct view for a GET" in {
      val result = controller(getMandatoryTrusteeCompany).onPageLoad(firstIndex)(fakeRequest)

      status(result) mustBe OK
      contentAsString(result) mustBe viewAsString
    }

    "redirect to Session Expired page" when {
      "GET" when {
        "trustee company name is not present" in {
          val result = controller(getEmptyData).onPageLoad(firstIndex)(fakeRequest)
          status(result) mustBe SEE_OTHER
          redirectLocation(result) mustBe Some(controllers.routes.SessionExpiredController.onPageLoad().url)
        }
      }
    }

  }

}