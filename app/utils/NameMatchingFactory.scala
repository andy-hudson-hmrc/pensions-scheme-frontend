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

package utils

import javax.inject.Inject

import connectors.PSANameCacheConnector
import identifiers.register.SchemeDetailsId
import models.register.SchemeDetails
import models.requests.DataRequest
import play.api.libs.json.JsValue
import play.api.mvc.AnyContent
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

class NameMatchingFactory @Inject()(
                                   pSANameCacheConnector: PSANameCacheConnector
                                   ){

  private def retrieveSchemeName(implicit request: DataRequest[AnyContent]): Option[SchemeDetails] =
    request.userAnswers.get(SchemeDetailsId)

  private def retrievePSAName(implicit request: DataRequest[AnyContent], ec: ExecutionContext, hc: HeaderCarrier): Future[Option[JsValue]] =
    pSANameCacheConnector.fetch(request.externalId)

  def nameMatching(implicit request: DataRequest[AnyContent], ec: ExecutionContext, hc: HeaderCarrier): Future[Option[NameMatching]] =
    retrievePSAName map { psaOpt =>
      for {
        psaName <- psaOpt
        schemeName <- retrieveSchemeName
      } yield {
        ???
      }
    }

}
