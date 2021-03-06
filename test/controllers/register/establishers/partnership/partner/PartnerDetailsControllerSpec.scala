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

import connectors.DataCacheConnector
import controllers.ControllerSpecBase
import controllers.actions.{DataRequiredActionImpl, DataRetrievalAction, FakeAuthAction, FakeDataRetrievalAction}
import forms.register.PersonDetailsFormProvider
import identifiers.register.SchemeDetailsId
import identifiers.register.establishers.partnership.PartnershipDetailsId
import identifiers.register.establishers.partnership.partner.PartnerDetailsId
import identifiers.register.establishers.{EstablishersId, IsEstablisherCompleteId}
import models.person.PersonDetails
import models.register.{SchemeDetails, SchemeType}
import models.{Index, NormalMode, PartnershipDetails}
import org.joda.time.LocalDate
import org.mockito.Matchers.{eq => eqTo, _}
import org.mockito.Mockito._
import org.scalatest.mockito.MockitoSugar
import play.api.data.Form
import play.api.libs.json.Json
import play.api.mvc.Call
import play.api.test.Helpers._
import utils.{FakeNavigator, SectionComplete, UserAnswers}
import views.html.register.establishers.partnership.partner.partnerDetails

import scala.concurrent.Future

class PartnerDetailsControllerSpec extends ControllerSpecBase {

  import PartnerDetailsControllerSpec._

  def controller(dataRetrievalAction: DataRetrievalAction = getMandatoryEstablisherPartnership): PartnerDetailsController =
    new PartnerDetailsController(
      frontendAppConfig,
      messagesApi,
      mockDataCacheConnector,
      new FakeNavigator(desiredRoute = onwardRoute),
      FakeAuthAction,
      dataRetrievalAction,
      new DataRequiredActionImpl,
      formProvider,
      mockSectionComplete)

  def viewAsString(form: Form[_] = form): String = partnerDetails(
    frontendAppConfig,
    form,
    NormalMode,
    firstEstablisherIndex,
    firstPartnerIndex,
    partnershipName
  )(fakeRequest, messages).toString

  private val postRequest = fakeRequest.withFormUrlEncodedBody(("firstName", "testFirstName"), ("lastName", "testLastName"),
    ("date.day", day.toString), ("date.month", month.toString), ("date.year", year.toString))

  "PartnerDetails Controller" must {

    "return OK and the correct view for a GET" in {
      val result = controller().onPageLoad(NormalMode, firstEstablisherIndex, firstPartnerIndex)(fakeRequest)

      status(result) mustBe OK
      contentAsString(result) mustBe viewAsString()
    }

    "populate the view correctly on a GET when the question has previously been answered" in {
      val validData = Json.obj(
        EstablishersId.toString -> Json.arr(
          Json.obj(
            PartnershipDetailsId.toString -> PartnershipDetails(partnershipName),
            "partner" -> Json.arr(
              Json.obj(
                PartnerDetailsId.toString ->
                  PersonDetails("First Name", Some("Middle Name"), "Last Name", new LocalDate(year, month, day))
              )
            )
          )
        )
      )
      val getRelevantData = new FakeDataRetrievalAction(Some(validData))

      val result = controller(getRelevantData).onPageLoad(NormalMode, firstEstablisherIndex, firstPartnerIndex)(fakeRequest)

      contentAsString(result) mustBe viewAsString(form.fill(PersonDetails("First Name", Some("Middle Name"), "Last Name", new LocalDate(year, month, day))))
    }

    "redirect to the next page when valid data is submitted" in {
      val validData = Json.obj(
        SchemeDetailsId.toString ->
          SchemeDetails("Test Scheme Name", SchemeType.SingleTrust),
        EstablishersId.toString -> Json.arr(
          Json.obj(
            PartnershipDetailsId.toString -> PartnershipDetails(partnershipName))
        )
      )

      when(mockDataCacheConnector.save(any(), any(), any())(any(), any(), any())).thenReturn(Future.successful(validData))

      val result = controller().onSubmit(NormalMode, firstEstablisherIndex, firstPartnerIndex)(postRequest)
      status(result) mustBe SEE_OTHER
      redirectLocation(result) mustBe Some(onwardRoute.url)
    }

    "return a Bad Request and errors when invalid data is submitted" in {
      val postRequest = fakeRequest.withFormUrlEncodedBody(("value", "invalid value"))
      val boundForm = form.bind(Map("value" -> "invalid value"))

      val result = controller().onSubmit(NormalMode, firstEstablisherIndex, firstPartnerIndex)(postRequest)

      status(result) mustBe BAD_REQUEST
      contentAsString(result) mustBe viewAsString(boundForm)
    }

    "redirect to Session Expired for a GET if no existing data is found" in {
      val result = controller(dontGetAnyData).onPageLoad(NormalMode, firstEstablisherIndex, firstPartnerIndex)(fakeRequest)

      status(result) mustBe SEE_OTHER
      redirectLocation(result) mustBe Some(controllers.routes.SessionExpiredController.onPageLoad().url)
    }

    "redirect to Session Expired for a POST if no existing data is found" in {
      val result = controller(dontGetAnyData).onSubmit(NormalMode, firstEstablisherIndex, firstPartnerIndex)(postRequest)

      status(result) mustBe SEE_OTHER
      redirectLocation(result) mustBe Some(controllers.routes.SessionExpiredController.onPageLoad().url)
    }

    "redirect to session expired from a GET when the index is invalid for establisher" in {
      val result = controller().onPageLoad(NormalMode, invalidIndex, firstPartnerIndex)(fakeRequest)

      status(result) mustBe SEE_OTHER
      redirectLocation(result) mustBe Some(controllers.routes.SessionExpiredController.onPageLoad().url)
    }

    "redirect to session expired from a POST when the index is invalid for establisher" in {
      val result = controller().onSubmit(NormalMode, invalidIndex, firstPartnerIndex)(fakeRequest)

      status(result) mustBe SEE_OTHER
      redirectLocation(result) mustBe Some(controllers.routes.SessionExpiredController.onPageLoad().url)
    }

    "set the establisher as not complete when the new partner is being added" in {
      reset(mockSectionComplete)
      val validData =
        Json.obj(
          SchemeDetailsId.toString ->
            SchemeDetails("Test Scheme Name", SchemeType.SingleTrust),
          EstablishersId.toString -> Json.arr(
            Json.obj(
              PartnershipDetailsId.toString -> PartnershipDetails(partnershipName),
              "partner" -> Json.arr(
                Json.obj(
                  PartnerDetailsId.toString ->
                    PersonDetails("First Name", Some("Middle Name"), "Last Name", new LocalDate(year, month, day))
                )
              )
            )
          )
        )
      val getRelevantData = new FakeDataRetrievalAction(Some(validData))
      val userAnswers = UserAnswers(validData)
      when(mockDataCacheConnector.save(any(), any(), any())(any(), any(), any())).thenReturn(Future.successful(validData))
      when(mockSectionComplete.setCompleteFlag(eqTo(IsEstablisherCompleteId(0)),
        eqTo(userAnswers), eqTo(false))(any(), any(), any())).thenReturn(Future.successful(userAnswers))

      val result = controller(getRelevantData).onSubmit(NormalMode, firstEstablisherIndex, firstPartnerIndex)(postRequest)
      status(result) mustBe SEE_OTHER
      verify(mockSectionComplete, times(1)).setCompleteFlag(eqTo(IsEstablisherCompleteId(0)), eqTo(userAnswers), eqTo(false))(any(), any(), any())
    }
  }
}

object PartnerDetailsControllerSpec extends MockitoSugar {
  def onwardRoute: Call = controllers.routes.IndexController.onPageLoad()

  val formProvider: PersonDetailsFormProvider = new PersonDetailsFormProvider()
  val form: Form[PersonDetails] = formProvider()

  val firstEstablisherIndex: Index = Index(0)
  val firstPartnerIndex: Index = Index(0)
  val invalidIndex: Index = Index(10)

  val partnershipName: String = "test partnership name"
  val mockDataCacheConnector: DataCacheConnector = mock[DataCacheConnector]
  val mockSectionComplete: SectionComplete = mock[SectionComplete]

  val day: Int = LocalDate.now().getDayOfMonth
  val month: Int = LocalDate.now().getMonthOfYear
  val year: Int = LocalDate.now().getYear - 20
}
