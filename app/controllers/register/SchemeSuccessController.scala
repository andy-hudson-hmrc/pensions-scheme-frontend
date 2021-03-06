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

package controllers.register

import config.FrontendAppConfig
import connectors.DataCacheConnector
import controllers.Retrievals
import controllers.actions._
import identifiers.register.{SchemeDetailsId, SubmissionReferenceNumberId}
import javax.inject.Inject
import models.register.SchemeType.MasterTrust
import models.requests.DataRequest
import org.joda.time.LocalDate
import play.api.Logger
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent}
import uk.gov.hmrc.play.bootstrap.controller.FrontendController
import views.html.register.schemeSuccess

import scala.concurrent.Future
import scala.util.Failure

class SchemeSuccessController @Inject()(appConfig: FrontendAppConfig,
                                        override val messagesApi: MessagesApi,
                                        cacheConnector: DataCacheConnector,
                                        authenticate: AuthAction,
                                        getData: DataRetrievalAction,
                                        requireData: DataRequiredAction) extends FrontendController with I18nSupport with Retrievals {

  def onPageLoad: Action[AnyContent] = (authenticate andThen getData andThen requireData).async {
    implicit request =>

      SubmissionReferenceNumberId.retrieve.right.map {
        submissionReferenceNumber =>
          val schemeName = request.userAnswers.get(SchemeDetailsId).map(_.schemeName)

          cacheConnector.removeAll(request.externalId) andThen {
            case Failure(t: Throwable) => Logger.warn("Could not remove scheme data following successful submission.", t)
          }

          Future.successful(
            Ok(
              schemeSuccess(
                appConfig,
                schemeName,
                LocalDate.now(),
                submissionReferenceNumber.schemeReferenceNumber,
                showMasterTrustContent
              )
            )
          )
      }
  }

  def onSubmit: Action[AnyContent] = authenticate {
    implicit request =>
      Redirect(controllers.routes.SchemesOverviewController.onPageLoad())
  }


  private def showMasterTrustContent(implicit request: DataRequest[AnyContent]): Boolean =
    appConfig.allowMasterTrust & request.userAnswers.get(SchemeDetailsId).map(_.schemeType).contains(MasterTrust)
}
