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

import connectors.FakeDataCacheConnector
import controllers.ControllerSpecBase
import controllers.actions._
import forms.address.AddressListFormProvider
import identifiers.register.SchemeDetailsId
import identifiers.register.trustees.TrusteesId
import identifiers.register.trustees.partnership.PartnershipPreviousAddressPostcodeLookupId
import models.address.TolerantAddress
import models.register.{SchemeDetails, SchemeType}
import models.{Index, NormalMode, PartnershipDetails}
import play.api.data.Form
import play.api.libs.json._
import play.api.mvc.Call
import play.api.test.Helpers._
import utils.FakeNavigator
import viewmodels.address.AddressListViewModel
import views.html.address.addressList

class PartnershipPreviousAddressListControllerSpec extends ControllerSpecBase {

  def onwardRoute: Call = controllers.routes.IndexController.onPageLoad()

  val formProvider = new AddressListFormProvider()
  val form = formProvider(Seq(0, 1))
  val index = Index(0)
  val partnershipName = "test partnership name"
  val schemeName = "test scheme name"
  val previousAddressTitle = "Select the previous address"
  val previousAddressHeading = "Select the previous address"
  val addresses = Seq(
    address("test post code 1"),
    address("test post code 2")
  )

  def address(postCode: String): TolerantAddress = TolerantAddress(
    Some("address line 1"),
    Some("address line 2"),
    Some("test town"),
    Some("test county"),
    Some(postCode),
    Some("United Kingdom")
  )

  val validData: JsObject = Json.obj(
    SchemeDetailsId.toString ->
      SchemeDetails(schemeName, SchemeType.SingleTrust),
    TrusteesId.toString -> Json.arr(
      Json.obj(
        "partnershipDetails" -> PartnershipDetails(partnershipName, false),
        PartnershipPreviousAddressPostcodeLookupId.toString -> addresses
      )
    )
  )

  def controller(dataRetrievalAction: DataRetrievalAction = getMandatoryTrusteePartnership): PartnershipPreviousAddressListController =
    new PartnershipPreviousAddressListController(
      frontendAppConfig,
      messagesApi,
      FakeDataCacheConnector,
      new FakeNavigator(desiredRoute = onwardRoute),
      FakeAuthAction,
      dataRetrievalAction, new DataRequiredActionImpl
    )

  def viewAsString(form: Form[_] = form): String =
    addressList(
      frontendAppConfig,
      form,
      AddressListViewModel(
        routes.PartnershipPreviousAddressListController.onSubmit(NormalMode, index),
        routes.PartnershipPreviousAddressController.onPageLoad(NormalMode, index),
        addresses,
        title = previousAddressTitle,
        heading = previousAddressHeading,
        subHeading = Some(partnershipName)

      )
    )(fakeRequest, messages).toString

  "PartnershipPreviousAddressList Controller" must {

    "return OK and the correct view for a GET" in {
      val getData = new FakeDataRetrievalAction(Some(validData))
      val result = controller(getData).onPageLoad(NormalMode, index)(fakeRequest)

      status(result) mustBe OK
      contentAsString(result) mustBe viewAsString()
    }

    "redirect to the next page when valid data is submitted" in {
      val postRequest = fakeRequest.withFormUrlEncodedBody(("value", "0"))
      val getData = new FakeDataRetrievalAction(Some(validData))
      val result = controller(getData).onSubmit(NormalMode, index)(postRequest)

      status(result) mustBe SEE_OTHER
      redirectLocation(result) mustBe Some(onwardRoute.url)
    }

    "redirect to postcode lookup when no address results exist (get)" in {
      val result = controller().onPageLoad(NormalMode, 0)(fakeRequest)
      status(result) mustEqual SEE_OTHER
      redirectLocation(result).value mustEqual
        controllers.register.trustees.partnership.routes.PartnershipPreviousAddressPostcodeLookupController.onPageLoad(NormalMode, 0).url
    }

    "redirect to postcode lookup when no address results exist (post)" in {
      val postRequest = fakeRequest.withFormUrlEncodedBody(("value", "0"))
      val result = controller().onSubmit(NormalMode, 0)(postRequest)
      status(result) mustEqual SEE_OTHER
      redirectLocation(result).value mustEqual
        controllers.register.trustees.partnership.routes.PartnershipPreviousAddressPostcodeLookupController.onPageLoad(NormalMode, 0).url
    }

    "return a Bad Request and errors when invalid data is submitted" in {
      val postRequest = fakeRequest.withFormUrlEncodedBody(("value", "invalid value"))
      val boundForm = form.bind(Map("value" -> "invalid value"))
      val getData = new FakeDataRetrievalAction(Some(validData))
      val result = controller(getData).onSubmit(NormalMode, index)(postRequest)

      status(result) mustBe BAD_REQUEST
      contentAsString(result) mustBe viewAsString(boundForm)
    }

    "redirect to Session Expired for a GET if no existing data is found" in {
      val result = controller(dontGetAnyData).onPageLoad(NormalMode, index)(fakeRequest)

      status(result) mustBe SEE_OTHER
      redirectLocation(result) mustBe Some(controllers.routes.SessionExpiredController.onPageLoad().url)
    }

    "redirect to Session Expired for a POST if no existing data is found" in {
      val postRequest = fakeRequest.withFormUrlEncodedBody(("value", "1"))
      val result = controller(dontGetAnyData).onSubmit(NormalMode, index)(postRequest)

      status(result) mustBe SEE_OTHER
      redirectLocation(result) mustBe Some(controllers.routes.SessionExpiredController.onPageLoad().url)
    }
  }
}
