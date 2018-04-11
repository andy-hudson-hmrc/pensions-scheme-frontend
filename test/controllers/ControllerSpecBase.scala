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

import base.SpecBase
import controllers.actions.FakeDataRetrievalAction
import identifiers.register.SchemeDetailsId
import identifiers.register.establishers.EstablishersId
import identifiers.register.establishers.company.CompanyDetailsId
import identifiers.register.establishers.company.director.DirectorDetailsId
import identifiers.register.establishers.individual.EstablisherDetailsId
import identifiers.register.trustees.TrusteesId
import models.CompanyDetails
import models.register.establishers.company.director.DirectorDetails
import models.register.establishers.individual.EstablisherDetails
import models.register.{SchemeDetails, SchemeType}
import org.joda.time.LocalDate
import play.api.libs.json.Json
import utils.{DateHelper, Enumerable, MapFormats}

trait ControllerSpecBase extends SpecBase with Enumerable.Implicits with MapFormats{

  val cacheMapId = "id"

  def getEmptyData: FakeDataRetrievalAction = new FakeDataRetrievalAction(Some(Json.obj()))

  def getMandatorySchemeName: FakeDataRetrievalAction = new FakeDataRetrievalAction(Some(Json.obj(
    SchemeDetailsId.toString -> SchemeDetails("Test Scheme Name", SchemeType.SingleTrust))))

  def dontGetAnyData: FakeDataRetrievalAction = new FakeDataRetrievalAction(None)

  def getMandatoryEstablisher: FakeDataRetrievalAction = new FakeDataRetrievalAction(Some(
    Json.obj(
      SchemeDetailsId.toString ->
        SchemeDetails("Test Scheme Name", SchemeType.SingleTrust),
      "establishers" -> Json.arr(
        Json.obj(
          EstablisherDetailsId.toString -> EstablisherDetails("test first name", None, "test last name", LocalDate.now())
        )
      )
    )))

  def getMandatoryTrustee: FakeDataRetrievalAction = new FakeDataRetrievalAction(Some(
    Json.obj(
    SchemeDetails.toString ->
      SchemeDetails("Test Scheme Name", SchemeType.SingleTrust),
    "trustees" -> Json.arr(
      Json.obj(
        TrusteeDetailsId.toString ->
          PersonDetails("Test", Some("Trustee"), "Name", LocalDate.now)
      )
    )
  )))

  def getMandatoryTrusteeCompany: FakeDataRetrievalAction = new FakeDataRetrievalAction(
    Some(Json.obj(
      SchemeDetailsId.toString ->
        SchemeDetails("Test Scheme Name", SchemeType.SingleTrust),
      TrusteesId.toString -> Json.arr(
        Json.obj(
          CompanyDetailsId.toString ->
            CompanyDetails("test company name", Some("123456"), Some("abcd"))
        )
      )
    ))
  )

  def getMandatoryEstablisherCompany: FakeDataRetrievalAction = new FakeDataRetrievalAction(
    Some(Json.obj(
      SchemeDetailsId.toString ->
        SchemeDetails("Test Scheme Name", SchemeType.SingleTrust),
      EstablishersId.toString -> Json.arr(
        Json.obj(
          CompanyDetailsId.toString ->
            CompanyDetails("test company name", Some("123456"), Some("abcd"))
        )
      )
    ))
  )

  def getMandatoryEstablisherCompanyDirector: FakeDataRetrievalAction = new FakeDataRetrievalAction(
    Some(Json.obj(
      SchemeDetailsId.toString ->
        SchemeDetails("Test Scheme Name", SchemeType.SingleTrust),
      EstablishersId.toString -> Json.arr(
        Json.obj(
          CompanyDetailsId.toString ->
            CompanyDetails("test company name", Some("123456"), Some("abcd")),
          "director" -> Json.arr(
            Json.obj(
              DirectorDetailsId.toString -> DirectorDetails("first", Some("middle"), "last",
                new LocalDate(1990, 2, 2))
            )
          )
        )
      )
    ))
  )
}
