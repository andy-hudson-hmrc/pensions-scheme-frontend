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
import controllers.actions._
import forms.ContactDetailsFormProvider
import identifiers.register.establishers.partnership.partner.{PartnerContactDetailsId, PartnerDetailsId}
import javax.inject.Inject
import models.{Index, Mode}
import play.api.i18n.MessagesApi
import play.api.mvc.{Action, AnyContent}
import utils._
import utils.annotations.EstablishersPartner
import viewmodels.{ContactDetailsViewModel, Message}

class PartnerContactDetailsController @Inject()(
                                                 @EstablishersPartner override val navigator: Navigator,
                                                 override val appConfig: FrontendAppConfig,
                                                 override val messagesApi: MessagesApi,
                                                 override val cacheConnector: DataCacheConnector,
                                                 authenticate: AuthAction,
                                                 getData: DataRetrievalAction,
                                                 requireData: DataRequiredAction,
                                                 formProvider: ContactDetailsFormProvider
                                               ) extends controllers.ContactDetailsController {

  private val form = formProvider()

  def onPageLoad(mode: Mode, establisherIndex: Index, partnerIndex: Index): Action[AnyContent] = (authenticate andThen getData andThen requireData).async {
    implicit request =>
      PartnerDetailsId(establisherIndex, partnerIndex).retrieve.right.map {
        partner =>
          get(PartnerContactDetailsId(establisherIndex, partnerIndex), form, viewmodel(mode, establisherIndex, partnerIndex, partner.fullName))
      }
  }

  def onSubmit(mode: Mode, establisherIndex: Index, partnerIndex: Index): Action[AnyContent] = (authenticate andThen getData andThen requireData).async {
    implicit request =>
      PartnerDetailsId(establisherIndex, partnerIndex).retrieve.right.map {
        partner =>
          post(PartnerContactDetailsId(establisherIndex, partnerIndex), mode, form, viewmodel(mode, establisherIndex, partnerIndex, partner.fullName))
      }
  }

  private def viewmodel(mode: Mode, establisherIndex: Index, partnerIndex: Index, partnerName: String) = ContactDetailsViewModel(
    postCall = routes.PartnerContactDetailsController.onSubmit(mode, establisherIndex, partnerIndex),
    title = Message("messages__partner_contact__title"),
    heading = Message("messages__partner_contact__heading"),
    body = Message("messages__partner_contact__body"),
    subHeading = Some(partnerName)
  )
}
