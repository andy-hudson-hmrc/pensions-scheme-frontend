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

import base.CSRFRequest
import connectors.{DataCacheConnector, FakeDataCacheConnector}
import controllers.ControllerSpecBase
import controllers.actions.{AuthAction, DataRetrievalAction, FakeAuthAction}
import forms.PayeFormProvider
import models.{Index, NormalMode}
import org.scalatest.MustMatchers
import play.api.Application
import play.api.http.Writeable
import play.api.inject.bind
import play.api.mvc.{Call, Request, Result}
import play.api.test.FakeRequest
import play.api.test.Helpers.{contentAsString, redirectLocation, route, running, status, _}
import utils.annotations.TrusteesPartnership
import utils.{FakeNavigator, Navigator}
import viewmodels.{Message, PayeViewModel}
import views.html.paye

import scala.concurrent.Future

class PartnershipPayeControllerSpec extends ControllerSpecBase with MustMatchers with CSRFRequest {

  import PartnershipPayeControllerSpec._

  "PartnershipPayeController" must {

    "render the view correctly on a GET request" in {
      requestResult(
        implicit app => addToken(FakeRequest(routes.PartnershipPayeController.onPageLoad(NormalMode, firstIndex))),
        (request, result) => {
          status(result) mustBe OK
          contentAsString(result) mustBe paye(frontendAppConfig, form, viewModel)(request, messages).toString()
        }
      )
    }

    "redirect to the next page on a POST request" in {
      requestResult(
        implicit app => addToken(FakeRequest(routes.PartnershipPayeController.onSubmit(NormalMode, firstIndex))
          .withFormUrlEncodedBody(("paye.hasPaye", "true"), ("paye.paye", "123456789"))),
        (_, result) => {
          status(result) mustBe SEE_OTHER
          redirectLocation(result) mustBe Some(onwardRoute.url)
        }
      )
    }

  }

}

object PartnershipPayeControllerSpec extends PartnershipPayeControllerSpec {

  val form = new PayeFormProvider()()
  val firstIndex = Index(0)

  def onwardRoute: Call = controllers.routes.IndexController.onPageLoad()

  val viewModel = PayeViewModel(
    routes.PartnershipPayeController.onSubmit(NormalMode, firstIndex),
    title = Message("messages__partnershipPaye__title"),
    heading = Message("messages__partnershipPaye__heading"),
    hint = Some(Message("messages__common__paye_hint")),
    subHeading = Some(Message("test partnership name"))
  )

  private def requestResult[T](request: Application => Request[T], test: (Request[_], Future[Result]) => Unit)
                              (implicit writeable: Writeable[T]): Unit = {

    running(_.overrides(
      bind[AuthAction].to(FakeAuthAction),
      bind[DataRetrievalAction].toInstance(getMandatoryTrusteePartnership),
      bind(classOf[Navigator]).qualifiedWith(classOf[TrusteesPartnership]).toInstance(new FakeNavigator(onwardRoute)),
      bind[DataCacheConnector].toInstance(FakeDataCacheConnector)
    )) {
      app =>
        val req = request(app)
        val result = route[T](app, req).value
        test(req, result)
    }
  }

}
