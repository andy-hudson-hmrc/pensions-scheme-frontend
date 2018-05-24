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

package controllers

import connectors.{DataCacheConnector, MongoCacheConnector}
import controllers.actions._
import org.joda.time.{DateTime, DateTimeZone, LocalDate}
import org.mockito.Matchers
import org.mockito.Mockito.when
import org.scalatest.mockito.MockitoSugar
import play.api.libs.json.Json
import play.api.test.Helpers.{contentAsString, _}
import views.html.schemesOverview

import scala.concurrent.Future

class SchemesOverviewControllerSpec extends ControllerSpecBase with MockitoSugar {

  val fakeCacheConnector: DataCacheConnector = mock[MongoCacheConnector]

  def controller(dataRetrievalAction: DataRetrievalAction = getMandatorySchemeName): SchemesOverviewController =
    new SchemesOverviewController(frontendAppConfig, messagesApi, fakeCacheConnector, FakeAuthAction,
      dataRetrievalAction, new DataRequiredActionImpl)

  val schemeName = "Test Scheme Name"
  val lastDate: DateTime = DateTime.now(DateTimeZone.UTC)
  val timestamp: Long = lastDate.getMillis
  val deleteDate = DateTime.now(DateTimeZone.UTC).plusDays(frontendAppConfig.daysDataSaved).toString

  def viewAsString(): String = schemesOverview(frontendAppConfig, Some(schemeName), Some(lastDate.toString), Some(deleteDate))(fakeRequest, messages).toString
  def viewAsStringNewScheme(): String = schemesOverview(frontendAppConfig, None, None, None)(fakeRequest, messages).toString

  "SchemesOverview Controller" must {

    "return OK and the correct view for a GET" when {
      "no scheme has been defined" in {
        val result = controller(getEmptyData).onPageLoad(fakeRequest)

        status(result) mustBe OK
        contentAsString(result) mustBe viewAsStringNewScheme()
      }

      "a scheme has been partially defined" in {
        val result = controller().onPageLoad(fakeRequest)
        when(fakeCacheConnector.fetchValue(Matchers.any(), Matchers.eq("lastUpdated"))(Matchers.any(), Matchers.any()))
          .thenReturn(Future.successful(Some(Json.parse(timestamp.toString))))

        status(result) mustBe OK
        contentAsString(result) mustBe viewAsString()
      }
    }
  }
}




