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

package controllers.register.establishers

import config.FrontendAppConfig
import controllers.Retrievals
import controllers.actions.{AuthAction, DataRequiredAction, DataRetrievalAction}
import identifiers.register.establishers.company.CompanyDetailsId
import identifiers.register.establishers.individual.EstablisherDetailsId
import javax.inject.Inject
import models.register.establishers.EstablisherKind
import models.register.establishers.EstablisherKind.{Company, Indivdual, Partnership}
import models.requests.DataRequest
import models.{Index, NormalMode}
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent, Result}
import uk.gov.hmrc.play.bootstrap.controller.FrontendController
import utils.Enumerable
import viewmodels.{AlreadyDeletedViewModel, Message}
import views.html.alreadyDeleted

import scala.concurrent.Future

class AlreadyDeletedController @Inject()(
                                          appConfig: FrontendAppConfig,
                                          override val messagesApi: MessagesApi,
                                          authenticate: AuthAction,
                                          getData: DataRetrievalAction,
                                          requireData: DataRequiredAction
                                        ) extends FrontendController with Retrievals with I18nSupport with Enumerable.Implicits {

  def onPageLoad(index: Index, establisherKind: EstablisherKind): Action[AnyContent] = (authenticate andThen getData andThen requireData).async {
    implicit request =>
      establisherName(index, establisherKind) match {
        case Right(establisherName) =>
          Future.successful(Ok(alreadyDeleted(appConfig, vm(establisherName))))
        case Left(result) => result
      }
  }

  private def vm(establisherName: String) = AlreadyDeletedViewModel(
    title = Message("messages__alreadyDeleted__establisher_title"),
    deletedEntity = establisherName,
    returnCall = controllers.register.establishers.routes.AddEstablisherController.onPageLoad(NormalMode)
  )

  private def establisherName(index: Index, establisherKind: EstablisherKind)(implicit dataRequest: DataRequest[AnyContent]): Either[Future[Result], String] = {
    establisherKind match {
      case Company => CompanyDetailsId(index).retrieve.right.map(_.companyName)
      case Indivdual => EstablisherDetailsId(index).retrieve.right.map(_.fullName)
      case Partnership => Left(Future.successful(SeeOther(controllers.routes.SessionExpiredController.onPageLoad().url)))
      case _ => Left(Future.successful(SeeOther(controllers.routes.SessionExpiredController.onPageLoad().url)))
    }
  }
}
