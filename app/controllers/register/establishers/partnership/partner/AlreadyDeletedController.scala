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
import controllers.Retrievals
import controllers.actions.{AuthAction, DataRequiredAction, DataRetrievalAction}
import identifiers.register.establishers.partnership.partner.PartnerDetailsId
import javax.inject.Inject
import models.Index
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent}
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

  def onPageLoad(establisherIndex: Index, partnerIndex: Index): Action[AnyContent] = (authenticate andThen getData andThen requireData).async {
    implicit request =>
      PartnerDetailsId(establisherIndex, partnerIndex).retrieve.right.map {
        details =>
          Future.successful(Ok(alreadyDeleted(appConfig, vm(establisherIndex, details.fullName))))
      }

  }

  private def vm(establisherIndex: Index, partnerName: String) = AlreadyDeletedViewModel(
    Message("messages__alreadyDeleted__partner_title"),
    partnerName,
    controllers.register.establishers.partnership.routes.AddPartnersController.onPageLoad(establisherIndex)
  )
}
