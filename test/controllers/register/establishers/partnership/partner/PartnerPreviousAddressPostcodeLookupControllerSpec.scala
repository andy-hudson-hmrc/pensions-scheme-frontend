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

package controllers.register.establishers.partnership.partner

import base.CSRFRequest
import config.FrontendAppConfig
import connectors.{AddressLookupConnector, DataCacheConnector, FakeDataCacheConnector}
import controllers.ControllerSpecBase
import controllers.actions._
import forms.address.PostCodeLookupFormProvider
import models.address.TolerantAddress
import models.person.PersonDetails
import models.{Index, NormalMode}
import org.joda.time.LocalDate
import org.mockito.Matchers
import org.mockito.Mockito.when
import org.scalatest.mockito.MockitoSugar
import play.api.i18n.MessagesApi
import play.api.inject.bind
import play.api.mvc.Call
import play.api.test.FakeRequest
import play.api.test.Helpers._
import utils.{FakeNavigator, Navigator}
import viewmodels.Message
import viewmodels.address.PostcodeLookupViewModel
import views.html.address.postcodeLookup

import scala.concurrent.Future

class PartnerPreviousAddressPostcodeLookupControllerSpec extends ControllerSpecBase with MockitoSugar with CSRFRequest {

  def onwardRoute: Call = routes.PartnerPreviousAddressPostcodeLookupController.onSubmit(NormalMode, establisherIndex, partnerIndex)

  def manualInputCall: Call = routes.PartnerPreviousAddressController.onPageLoad(NormalMode, establisherIndex, partnerIndex)

  private val formProvider = new PostCodeLookupFormProvider()

  private val establisherIndex = Index(0)
  private val partnerIndex = Index(0)

  private val partner = PersonDetails("first", Some("middle"), "last", LocalDate.now())

  private val form = formProvider()
  private val fakeAddressLookupConnector: AddressLookupConnector = mock[AddressLookupConnector]
  private val fakeCacheConnector: DataCacheConnector = mock[DataCacheConnector]


  lazy val viewmodel = PostcodeLookupViewModel(
    onwardRoute,
    manualInputCall,
    Message("messages__partnerPreviousAddressPostcodeLookup__title"),
    Message("messages__partnerPreviousAddressPostcodeLookup__heading"),
    Some(partner.fullName),
    Some(Message("messages__partnerPreviousAddressPostcodeLookup__lede"))
  )

  "PartnerPreviousAddressPostcodeLookup Controller" must {

    "return OK and the correct view for a GET" in {

      val call: Call = routes.PartnerPreviousAddressPostcodeLookupController.onPageLoad(NormalMode, establisherIndex, partnerIndex)
      running(_.overrides(
        bind[FrontendAppConfig].to(frontendAppConfig),
        bind[Navigator].toInstance(FakeNavigator),
        bind[DataCacheConnector].toInstance(fakeCacheConnector),
        bind[AddressLookupConnector].toInstance(fakeAddressLookupConnector),
        bind[AuthAction].to(FakeAuthAction),
        bind[DataRetrievalAction].to(getMandatoryEstablisherPartner)
      )) {
        implicit app =>

          val request = addToken(FakeRequest(call)
            .withHeaders("Csrf-Token" -> "nocheck"))

          val result = route(app, request).get
          status(result) mustBe OK

          contentAsString(result) mustEqual postcodeLookup(
            frontendAppConfig,
            form,
            viewmodel
          )(request, messages).toString
      }
    }

    "redirect to the next page on POST request" in {

      val validPostcode = "ZZ1 1ZZ"
      val onwardUrl = routes.PartnerPreviousAddressListController.onSubmit(NormalMode, establisherIndex, partnerIndex)

      val fakeRequest = addToken(FakeRequest(onwardUrl)
        .withFormUrlEncodedBody("value" -> validPostcode))
        .withHeaders("Csrf-Token" -> "nocheck")

      when(fakeAddressLookupConnector.addressLookupByPostCode(Matchers.eq(validPostcode))(Matchers.any(), Matchers.any()))
        .thenReturn(Future.successful(Seq(TolerantAddress(Some("address line 1"), Some("address line 2"), None, None, Some(validPostcode), Some("GB")))
        ))

      running(_.overrides(
        bind[FrontendAppConfig].to(frontendAppConfig),
        bind[MessagesApi].to(messagesApi),
        bind[Navigator].toInstance(new FakeNavigator(desiredRoute = onwardRoute)),
        bind[DataCacheConnector].toInstance(FakeDataCacheConnector),
        bind[AddressLookupConnector].toInstance(fakeAddressLookupConnector),
        bind[AuthAction].to(FakeAuthAction),
        bind[DataRetrievalAction].to(getMandatoryEstablisherPartner),
        bind[DataRequiredAction].to(new DataRequiredActionImpl),
        bind[PostCodeLookupFormProvider].to(formProvider)
      )) {
        app =>
          val result = route(app, fakeRequest).get

          status(result) mustBe SEE_OTHER
          redirectLocation(result) mustBe Some(onwardRoute.url)
      }
    }

  }
}
