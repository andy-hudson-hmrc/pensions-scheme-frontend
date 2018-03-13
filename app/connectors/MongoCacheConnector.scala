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

package connectors

import com.google.inject.Inject
import identifiers.TypedIdentifier
import play.api.libs.json._
import repositories.SessionRepository
import uk.gov.hmrc.http.HeaderCarrier
import utils.{Cleanup, UserAnswers}

import scala.concurrent.{ExecutionContext, Future}

class MongoCacheConnector @Inject() (
                                      val sessionRepository: SessionRepository
                                    ) extends DataCacheConnector {

  override def save[A, I <: TypedIdentifier[A]](cacheId: String, id: I, value: A)
                                               (implicit
                                                fmt: Format[A],
                                                cu: Cleanup[I],
                                                ec: ExecutionContext,
                                                hc: HeaderCarrier
                                               ): Future[JsValue] = {

    sessionRepository().get(cacheId).flatMap {
      json =>
        UserAnswers(json.getOrElse(Json.obj())).set(id)(value) match {
          case JsSuccess(UserAnswers(updatedJson), _) =>
            sessionRepository().upsert(cacheId, updatedJson)
              .map(_ => updatedJson)
          case JsError(errors) =>
            Future.failed(JsResultException(errors))
        }
    }
  }

  override def fetch(cacheId: String)(implicit
                                      ec: ExecutionContext,
                                      hc: HeaderCarrier
                                     ): Future[Option[JsValue]] = {

    sessionRepository().get(cacheId)
  }
}
