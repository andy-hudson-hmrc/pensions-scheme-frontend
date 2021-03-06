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

package controllers.register.trustees.partnership

import connectors.{AddressLookupConnector, FakeDataCacheConnector}
import controllers.ControllerSpecBase
import controllers.actions._
import forms.address.PostCodeLookupFormProvider
import identifiers.register.SchemeDetailsId
import identifiers.register.trustees.TrusteesId
import identifiers.register.trustees.partnership.{PartnershipDetailsId, PartnershipPreviousAddressPostcodeLookupId}
import models.address.TolerantAddress
import models.register.{SchemeDetails, SchemeType}
import models.{Index, NormalMode, PartnershipDetails}
import org.mockito.Mockito._
import org.mockito._
import org.scalatest.mockito.MockitoSugar
import play.api.data.Form
import play.api.libs.json._
import play.api.mvc.Call
import play.api.test.Helpers._
import uk.gov.hmrc.http.HeaderCarrier
import utils.FakeNavigator
import viewmodels.Message
import viewmodels.address.PostcodeLookupViewModel
import views.html.address.postcodeLookup

import scala.concurrent.Future

class PartnershipPreviousAddressPostcodeLookupControllerSpec extends ControllerSpecBase with MockitoSugar {

  def onwardRoute: Call = controllers.routes.IndexController.onPageLoad()

  val formProvider = new PostCodeLookupFormProvider()
  val form = formProvider()

  val fakeAddressLookupConnector: AddressLookupConnector = mock[AddressLookupConnector]

  val index = Index(0)
  val partnershipName: String = "test partnership name"

  implicit val hc: HeaderCarrier = mock[HeaderCarrier]

  private def fakeAddress(postCode: String) = TolerantAddress(
    Some("Address Line 1"),
    Some("Address Line 2"),
    Some("Address Line 3"),
    Some("Address Line 4"),
    Some(postCode),
    Some("GB")
  )

  private val testAnswer = "AB12 3CD"

  val validData: JsObject = Json.obj(
    SchemeDetailsId.toString ->
      SchemeDetails("Test Scheme Name", SchemeType.SingleTrust),
    TrusteesId.toString -> Json.arr(
      Json.obj(
        PartnershipDetailsId.toString ->
          PartnershipDetails("test partnership name", false),
        PartnershipPreviousAddressPostcodeLookupId.toString ->
          Seq(fakeAddress(testAnswer))
      ),
      Json.obj(
        PartnershipDetailsId.toString ->
          PartnershipDetails("test", false)
      )
    )
  )

  def controller(dataRetrievalAction: DataRetrievalAction = getMandatoryTrusteePartnership): PartnershipPreviousAddressPostcodeLookupController =
    new PartnershipPreviousAddressPostcodeLookupController(
      frontendAppConfig,
      messagesApi,
      FakeDataCacheConnector,
      fakeAddressLookupConnector,
      new FakeNavigator(desiredRoute = onwardRoute),
      FakeAuthAction,
      dataRetrievalAction,
      new DataRequiredActionImpl,
      formProvider
    )

  def viewAsString(form: Form[_] = form): String =
    postcodeLookup(
      frontendAppConfig,
      form,
      PostcodeLookupViewModel(
        routes.PartnershipPreviousAddressPostcodeLookupController.onSubmit(NormalMode, index),
        routes.PartnershipPreviousAddressController.onPageLoad(NormalMode, index),
        Message("messages__partnershipPreviousAddressPostcodeLookup__title"),
        Message("messages__partnershipPreviousAddressPostcodeLookup__title"),
        Some(partnershipName)
      )
    )(fakeRequest, messages).toString

  "PartnershipPreviousAddressPostcodeLookup Controller" must {

    "return OK and the correct view for a GET" in {
      val result = controller().onPageLoad(NormalMode, index)(fakeRequest)

      status(result) mustBe OK
      contentAsString(result) mustBe viewAsString()
    }

    "not populate the view correctly on a GET when the question has previously been answered" in {
      val getRelevantData = new FakeDataRetrievalAction(Some(validData))

      val result = controller(getRelevantData).onPageLoad(NormalMode, index)(fakeRequest)

      contentAsString(result) mustBe viewAsString()
    }

    "redirect to the next page when valid data is submitted" in {
      val postRequest = fakeRequest.withFormUrlEncodedBody(("value", testAnswer))
      when(fakeAddressLookupConnector.addressLookupByPostCode(Matchers.eq(testAnswer))(Matchers.any(), Matchers.any()))
        .thenReturn(Future.successful(Seq(fakeAddress(testAnswer))))
      val result = controller().onSubmit(NormalMode, index)(postRequest)

      status(result) mustBe SEE_OTHER
      redirectLocation(result) mustBe Some(onwardRoute.url)
    }

    "return a Bad Request and errors when invalid data is submitted" in {
      val postRequest = fakeRequest.withFormUrlEncodedBody(("value", ""))
      val boundForm = form.bind(Map("value" -> ""))

      val result = controller().onSubmit(NormalMode, index)(postRequest)

      status(result) mustBe BAD_REQUEST
      contentAsString(result) mustBe viewAsString(boundForm)
    }

    "redirect to Session Expired for a GET if no existing data is found" in {
      val result = controller(dontGetAnyData).onPageLoad(NormalMode, index)(fakeRequest)

      status(result) mustBe SEE_OTHER
      redirectLocation(result) mustBe Some(controllers.routes.SessionExpiredController.onPageLoad().url)
    }

    "redirect to Session Expired for a POST if no existing data is found" in {
      val postRequest = fakeRequest.withFormUrlEncodedBody(("value", testAnswer))
      val result = controller(dontGetAnyData).onSubmit(NormalMode, index)(postRequest)

      status(result) mustBe SEE_OTHER
      redirectLocation(result) mustBe Some(controllers.routes.SessionExpiredController.onPageLoad().url)
    }
  }
}
