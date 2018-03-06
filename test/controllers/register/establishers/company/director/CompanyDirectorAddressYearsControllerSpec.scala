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

package controllers.register.establishers.company.director

import play.api.data.Form
import play.api.libs.json.Json
import utils.FakeNavigator
import connectors.FakeDataCacheConnector
import controllers.actions._
import play.api.test.Helpers._
import models.{Index, NormalMode}
import controllers.ControllerSpecBase
import forms.register.establishers.company.director.CompanyDirectorAddressYearsFormProvider
import identifiers.register.establishers.EstablishersId
import identifiers.register.establishers.company.director.CompanyDirectorAddressYearsId
import models.register.establishers.company.director.CompanyDirectorAddressYears
import views.html.register.establishers.company.director.companyDirectorAddressYears

class CompanyDirectorAddressYearsControllerSpec extends ControllerSpecBase {

  def onwardRoute = controllers.routes.IndexController.onPageLoad()

  val formProvider = new CompanyDirectorAddressYearsFormProvider()
  val form = formProvider()
  val establisherIndex = Index(0)
  val directorIndex = Index (0)
  val invalidIndex = Index(10)
  val directorName: String = "Test Director Name"


  def controller(dataRetrievalAction: DataRetrievalAction = getEmptyData): CompanyDirectorAddressYearsController =
    new CompanyDirectorAddressYearsController(frontendAppConfig, messagesApi, FakeDataCacheConnector, new FakeNavigator(desiredRoute = onwardRoute),
      FakeAuthAction, dataRetrievalAction, new DataRequiredActionImpl, formProvider)

  def viewAsString(form: Form[_] = form): String = companyDirectorAddressYears(frontendAppConfig, form, NormalMode,
    establisherIndex, directorIndex)(fakeRequest, messages).toString

  val validData = Json.obj(
    EstablishersId.toString -> Json.arr(
      Json.obj(
        "director" -> Json.arr(
          Json.obj(
            CompanyDirectorAddressYearsId.toString ->
              CompanyDirectorAddressYears.options.head.value.toString
          )
        )
      )
    )
  )

  "CompanyDirectorAddressYears Controller" must {

    "return OK and the correct view for a GET" in {
      val result = controller().onPageLoad(NormalMode, establisherIndex, directorIndex, directorName)(fakeRequest)
      status(result) mustBe OK
      contentAsString(result) mustBe viewAsString()
    }

    "populate the view correctly on a GET when the question has previously been answered" in {
      val getRelevantData = new FakeDataRetrievalAction(Some(validData))
      val result = controller(getRelevantData).onPageLoad(NormalMode, establisherIndex, directorIndex, directorName)(fakeRequest)
      contentAsString(result) mustBe viewAsString(form.fill(CompanyDirectorAddressYears.values.head))
    }

    "redirect to session expired from a GET when the index is invalid" ignore {
      val getRelevantData = new FakeDataRetrievalAction(Some(validData))
      val result = controller(getRelevantData).onPageLoad(NormalMode, establisherIndex, invalidIndex, directorName)(fakeRequest)
      status(result) mustBe SEE_OTHER
      redirectLocation(result) mustBe Some(controllers.routes.SessionExpiredController.onPageLoad().url)
    }

    "redirect to the next page when valid data is submitted" in {
      val postRequest = fakeRequest.withFormUrlEncodedBody(("value", CompanyDirectorAddressYears.options.head.value))
      val result = controller().onSubmit(NormalMode, establisherIndex, directorIndex, directorName)(postRequest)
      status(result) mustBe SEE_OTHER
      redirectLocation(result) mustBe Some(onwardRoute.url)
    }

    "return a Bad Request and errors when invalid data is submitted" in {
      val postRequest = fakeRequest.withFormUrlEncodedBody(("value", "invalid value"))
      val boundForm = form.bind(Map("value" -> "invalid value"))
      val result = controller().onSubmit(NormalMode, establisherIndex, directorIndex, directorName)(postRequest)
      status(result) mustBe BAD_REQUEST
      contentAsString(result) mustBe viewAsString(boundForm)
    }

    "redirect to Session Expired for a GET if no existing data is found" in {
      val result = controller(dontGetAnyData).onPageLoad(NormalMode, establisherIndex, directorIndex, directorName)(fakeRequest)
      status(result) mustBe SEE_OTHER
      redirectLocation(result) mustBe Some(controllers.routes.SessionExpiredController.onPageLoad().url)
    }

    "redirect to Session Expired for a POST if no existing data is found" in {
      val postRequest = fakeRequest.withFormUrlEncodedBody(("value", CompanyDirectorAddressYears.options.head.value))
      val result = controller(dontGetAnyData).onSubmit(NormalMode, establisherIndex, directorIndex, directorName)(postRequest)
      status(result) mustBe SEE_OTHER
      redirectLocation(result) mustBe Some(controllers.routes.SessionExpiredController.onPageLoad().url)
    }
  }
}
