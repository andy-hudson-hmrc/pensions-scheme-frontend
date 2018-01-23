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

import uk.gov.hmrc.http.cache.client.CacheMap
import base.SpecBase
import controllers.actions.FakeDataRetrievalAction
import identifiers.register.SchemeDetailsId
import identifiers.register.establishers.individual.EstablisherDetailsId
import models.register.establishers.individual.{EstablisherDetails, EstablishersIndividualMap}
import models.register.{SchemeDetails, SchemeType}
import org.joda.time.LocalDate
import play.api.libs.json.Json
import utils.{Enumerable, MapFormats}

trait ControllerSpecBase extends SpecBase with Enumerable.Implicits with MapFormats{

  val cacheMapId = "id"

  def emptyCacheMap: CacheMap = CacheMap(cacheMapId, Map())

  def getEmptyCacheMap: FakeDataRetrievalAction = new FakeDataRetrievalAction(Some(emptyCacheMap))

  def getMandatorySchemeNameCacheMap: FakeDataRetrievalAction = new FakeDataRetrievalAction(Some(CacheMap("id", Map(
    SchemeDetailsId.toString -> Json.toJson(SchemeDetails("Test Scheme Name", SchemeType.SingleTrust))))))

  def dontGetAnyData: FakeDataRetrievalAction = new FakeDataRetrievalAction(None)

  def getMandatoryEstablisherCacheMap: FakeDataRetrievalAction = new FakeDataRetrievalAction(Some(CacheMap("id", Map(SchemeDetailsId.toString -> Json.toJson(
    SchemeDetails("Test Scheme Name", SchemeType.SingleTrust)), EstablisherDetailsId.toString ->
    Json.toJson(EstablishersIndividualMap[EstablisherDetails](Map(0 -> EstablisherDetails("test first name", "test last name", LocalDate.now())))
  )))))
}