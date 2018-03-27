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

import base.CSRFRequest
import config.FrontendAppConfig
import connectors.{DataCacheConnector, FakeDataCacheConnector}
import controllers.ControllerSpecBase
import controllers.actions._
import controllers.register.trustees.company.routes._
import forms.address.AddressFormProvider
import identifiers.register.trustees.TrusteesId
import identifiers.register.trustees.company.CompanyDetailsId
import models.address.Address
import models.{CompanyDetails, Index, NormalMode}
import org.scalatest.OptionValues
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.mockito.MockitoSugar
import play.api.data.Form
import play.api.i18n.MessagesApi
import play.api.inject.bind
import play.api.libs.json.Json
import play.api.mvc.Call
import play.api.test.FakeRequest
import play.api.test.Helpers._
import utils._
import viewmodels.address.ManualAddressViewModel
import views.html.address.manualAddress


class PreviousAddressControllerSpec extends ControllerSpecBase with MockitoSugar with ScalaFutures with CSRFRequest with OptionValues {

  def countryOptions: CountryOptions = new CountryOptions(options)

  val options = Seq(InputOption("territory:AE-AZ", "Abu Dhabi"), InputOption("country:AF", "Afghanistan"))

  val firstIndex = Index(0)

  val formProvider = new AddressFormProvider(FakeCountryOptions())
  val companyDetails = CompanyDetails("companyName", None, None)

  val form: Form[Address] = formProvider()

  val retrieval = new FakeDataRetrievalAction(Some(Json.obj(
    TrusteesId.toString -> Json.arr(Json.obj(CompanyDetailsId.toString -> companyDetails))
  )))

  "PreviousAddress Controller" must {

    "render manualAddress from GET request" in {

      running(_.overrides(
        bind[FrontendAppConfig].to(frontendAppConfig),
        bind[Navigator].toInstance(FakeNavigator),
        bind[DataCacheConnector].toInstance(FakeDataCacheConnector),
        bind[AuthAction].to(FakeAuthAction),
        bind[DataRetrievalAction].to(retrieval),
        bind[CountryOptions].to(countryOptions)
      )) {
        implicit app =>

          val viewmodel = ManualAddressViewModel(
            PreviousAddressController.onSubmit(NormalMode, firstIndex),
            countryOptions.options,
            "messages__companyAddress__title",
            "messages__companyAddress__heading",
            secondaryHeader = Some(companyDetails.companyName),
            hint = "messages__companyAddress__trustee__lede"
          )

          def viewAsString(form: Form[_] = form) = manualAddress(frontendAppConfig, form, viewmodel)(fakeRequest, messages).toString

          val request = addToken(
            FakeRequest(PreviousAddressController.onPageLoad(NormalMode, firstIndex))
            .withHeaders("Csrf-Token" -> "nocheck")
          )

          val result = route(app, request).value

          status(result) must be(OK)

          contentAsString(result) mustEqual manualAddress(
            frontendAppConfig,
            form,
            viewmodel
          )(request, messages).toString

      }

    }

    "redirect to next page on POST request" in {

      val onwardCall = Call("GET", "/")

      running(_.overrides(
        bind[FrontendAppConfig].to(frontendAppConfig),
        bind[MessagesApi].to(messagesApi),
        bind[DataCacheConnector].toInstance(FakeDataCacheConnector),
        bind[Navigator].toInstance(new FakeNavigator(desiredRoute = onwardCall)),
        bind[AuthAction].to(FakeAuthAction),
        bind[DataRetrievalAction].to(retrieval),
        bind[DataRequiredAction].to(new DataRequiredActionImpl),
        bind[AddressFormProvider].to(formProvider)
      )) {
        implicit app =>

          val fakeRequest = addToken(FakeRequest(PreviousAddressController.onSubmit(NormalMode, firstIndex))
            .withHeaders("Csrf-Token" -> "nocheck")
            .withFormUrlEncodedBody(
              ("addressLine1", "value 1"),
              ("addressLine2", "value 2"),
              ("postCode", "AB1 1AB"),
              "country" -> "GB"))

          val result = route(app, fakeRequest).value

          status(result) must be(SEE_OTHER)
          redirectLocation(result).value mustEqual onwardCall.url
      }
    }
  }
}
