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

import config.FrontendAppConfig
import connectors.DataCacheConnector
import controllers.Retrievals
import controllers.actions.{AuthAction, DataRequiredAction, DataRetrievalAction}
import identifiers.register.establishers.IsEstablisherCompleteId
import identifiers.register.establishers.partnership.PartnershipDetailsId
import identifiers.register.establishers.partnership.partner.{ConfirmDeletePartnerId, PartnerDetailsId}
import javax.inject.Inject
import models.{Index, NormalMode}
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent}
import uk.gov.hmrc.play.bootstrap.controller.FrontendController
import utils.annotations.EstablishersPartner
import utils.{Navigator, SectionComplete}
import views.html.register.establishers.partnership.partner.confirmDeletePartner

import scala.concurrent.Future

class ConfirmDeletePartnerController @Inject()(
                                                appConfig: FrontendAppConfig,
                                                override val messagesApi: MessagesApi,
                                                dataCacheConnector: DataCacheConnector,
                                                @EstablishersPartner navigator: Navigator,
                                                authenticate: AuthAction,
                                                getData: DataRetrievalAction,
                                                requireData: DataRequiredAction,
                                                sectionComplete: SectionComplete
                                              ) extends FrontendController with I18nSupport with Retrievals {

  def onPageLoad(establisherIndex: Index, partnerIndex: Index): Action[AnyContent] = (authenticate andThen getData andThen requireData).async {
    implicit request =>
      (PartnershipDetailsId(establisherIndex) and PartnerDetailsId(establisherIndex, partnerIndex)).retrieve.right.map {
        case partnership ~ partner =>
          if (partner.isDeleted) {
            Future.successful(Redirect(routes.AlreadyDeletedController.onPageLoad(establisherIndex, partnerIndex)))
          } else {
            Future.successful(
              Ok(
                confirmDeletePartner(
                  appConfig,
                  partnership.name,
                  partner.fullName,
                  routes.ConfirmDeletePartnerController.onSubmit(establisherIndex, partnerIndex),
                  controllers.register.establishers.partnership.routes.AddPartnersController.onPageLoad(establisherIndex)
                )
              )
            )
          }
      }
  }

  def onSubmit(establisherIndex: Index, partnerIndex: Index): Action[AnyContent] = (authenticate andThen getData andThen requireData).async {
    implicit request =>
      PartnerDetailsId(establisherIndex, partnerIndex).retrieve.right.map {
        partnerDetails =>
          dataCacheConnector.save(PartnerDetailsId(establisherIndex, partnerIndex), partnerDetails.copy(isDeleted = true)).flatMap {
            userAnswers =>
              if (userAnswers.allDirectorsAfterDelete(establisherIndex).isEmpty) {
                sectionComplete.setCompleteFlag(IsEstablisherCompleteId(establisherIndex), request.userAnswers, value = false).map { _ =>
                  Redirect(navigator.nextPage(ConfirmDeletePartnerId(establisherIndex), NormalMode, userAnswers))
                }
              } else {
                Future.successful(Redirect(navigator.nextPage(ConfirmDeletePartnerId(establisherIndex), NormalMode, userAnswers)))
              }
          }
      }
  }
}
