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

package controllers.register.trustees.partnership

import config.FrontendAppConfig
import connectors.DataCacheConnector
import controllers.Retrievals
import controllers.actions.{AuthAction, DataRequiredAction, DataRetrievalAction}
import controllers.address.AddressListController
import identifiers.register.trustees.partnership.{PartnershipAddressId, PartnershipAddressListId, PartnershipDetailsId, PartnershipPostcodeLookupId}
import javax.inject.Inject
import models.requests.DataRequest
import models.{Index, Mode}
import play.api.i18n.MessagesApi
import play.api.mvc.{Action, AnyContent, Result}
import utils.Navigator
import utils.annotations.TrusteesPartnership
import viewmodels.Message
import viewmodels.address.AddressListViewModel

import scala.concurrent.Future

class PartnershipAddressListController @Inject()(override val appConfig: FrontendAppConfig,
                                                 override val messagesApi: MessagesApi,
                                                 override val cacheConnector: DataCacheConnector,
                                                 @TrusteesPartnership override val navigator: Navigator,
                                                 authenticate: AuthAction,
                                                 getData: DataRetrievalAction,
                                                 requireData: DataRequiredAction) extends AddressListController with Retrievals {

  def onPageLoad(mode: Mode, index: Index): Action[AnyContent] = (authenticate andThen getData andThen requireData).async {
    implicit request =>
      viewmodel(mode, index).right.map(get)
  }

  def onSubmit(mode: Mode, index: Index): Action[AnyContent] = (authenticate andThen getData andThen requireData).async {
    implicit request =>
      viewmodel(mode, index).right.map {
        vm =>
          post(vm, PartnershipAddressListId(index), PartnershipAddressId(index), mode)
      }
  }

  private def viewmodel(mode: Mode, index: Index)(implicit request: DataRequest[AnyContent]): Either[Future[Result], AddressListViewModel] = {
    (PartnershipDetailsId(index) and PartnershipPostcodeLookupId(index)).retrieve.right.map {
      case partnershipDetails ~ addresses => AddressListViewModel(
        postCall = routes.PartnershipAddressListController.onSubmit(mode, index),
        manualInputCall = routes.PartnershipAddressController.onPageLoad(mode, index),
        addresses = addresses,
        subHeading = Some(Message(partnershipDetails.name))
      )
    }.left.map(_ =>
      Future.successful(Redirect(routes.PartnershipPostcodeLookupController.onPageLoad(mode, index))))
  }
}
