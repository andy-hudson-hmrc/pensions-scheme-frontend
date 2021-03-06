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

package controllers.register.adviser

import config.FrontendAppConfig
import connectors.DataCacheConnector
import controllers.Retrievals
import controllers.actions.{AuthAction, DataRequiredAction, DataRetrievalAction}
import controllers.address.AddressListController
import identifiers.register.adviser._
import javax.inject.Inject
import models.Mode
import models.requests.DataRequest
import play.api.i18n.MessagesApi
import play.api.mvc.{Action, AnyContent, Result}
import utils.Navigator
import utils.annotations.Adviser
import viewmodels.Message
import viewmodels.address.AddressListViewModel

import scala.concurrent.Future

class AdviserAddressListController @Inject()(override val appConfig: FrontendAppConfig,
                                             override val messagesApi: MessagesApi,
                                             override val cacheConnector: DataCacheConnector,
                                             @Adviser override val navigator: Navigator,
                                             authenticate: AuthAction,
                                             getData: DataRetrievalAction,
                                             requireData: DataRequiredAction) extends AddressListController with Retrievals {

  def onPageLoad(mode: Mode): Action[AnyContent] = (authenticate andThen getData andThen requireData).async {
    implicit request =>
      viewModel(mode).right.map(get)
  }

  def onSubmit(mode: Mode): Action[AnyContent] = (authenticate andThen getData andThen requireData).async {
    implicit request =>
      viewModel(mode).right.map {
        vm =>
          post(vm, AdviserAddressListId, AdviserAddressId, mode)
      }
  }

  private def viewModel(mode: Mode)(implicit request: DataRequest[AnyContent]): Either[Future[Result],
    AddressListViewModel] = {
    AdviserAddressPostCodeLookupId.retrieve.right.map {
      addresses =>
        AddressListViewModel(
          postCall = routes.AdviserAddressListController.onSubmit(mode),
          manualInputCall = routes.AdviserAddressController.onPageLoad(mode),
          addresses = addresses,
          subHeading = Some(Message("messages__adviserDetails__secondary_heading"))
        )
    }.left.map(_ => Future.successful(Redirect(routes.AdviserPostCodeLookupController.onPageLoad(mode))))
  }
}
