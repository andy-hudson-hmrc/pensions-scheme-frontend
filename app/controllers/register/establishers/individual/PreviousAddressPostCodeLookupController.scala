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

package controllers.register.establishers.individual

import config.FrontendAppConfig
import connectors.{AddressLookupConnector, DataCacheConnector}
import controllers.actions._
import controllers.address.{PostcodeLookupController => GenericPostcodeLookupController}
import forms.address.PostCodeLookupFormProvider
import identifiers.register.establishers.individual.{EstablisherDetailsId, PreviousPostCodeLookupId}
import javax.inject.Inject
import models.{Index, Mode}
import play.api.data.Form
import play.api.i18n.MessagesApi
import play.api.mvc.{Action, AnyContent}
import utils.Navigator
import utils.annotations.EstablishersIndividual
import viewmodels.Message
import viewmodels.address.PostcodeLookupViewModel

class PreviousAddressPostCodeLookupController @Inject()(
                                                         override val appConfig: FrontendAppConfig,
                                                         override val messagesApi: MessagesApi,
                                                         override val cacheConnector: DataCacheConnector,
                                                         override val addressLookupConnector: AddressLookupConnector,
                                                         @EstablishersIndividual override val navigator: Navigator,
                                                         authenticate: AuthAction,
                                                         getData: DataRetrievalAction,
                                                         requireData: DataRequiredAction,
                                                         formProvider: PostCodeLookupFormProvider
                                                       ) extends GenericPostcodeLookupController {

  private val title: Message = "messages__establisher_individual_previous_address__title"
  private val heading: Message = "messages__establisher_individual_previous_address__title"
  private val hint: Message = "messages__establisher_individual_previous_address_lede"

  protected val form: Form[String] = formProvider()

  private def viewmodel(index: Int, mode: Mode): Retrieval[PostcodeLookupViewModel] =
    Retrieval {
      implicit request =>
        EstablisherDetailsId(index).retrieve.right.map {
          details =>
            PostcodeLookupViewModel(
              routes.PreviousAddressPostCodeLookupController.onSubmit(mode, index),
              routes.PreviousAddressController.onPageLoad(mode, index),
              title = Message(title),
              heading = Message(heading),
              subHeading = Some(details.fullName),
              hint = Some(Message(hint))
            )
        }
    }

  def onPageLoad(mode: Mode, index: Index): Action[AnyContent] =
    (authenticate andThen getData andThen requireData).async {
      implicit request =>
        viewmodel(index, mode).retrieve.right map get
    }

  def onSubmit(mode: Mode, index: Index): Action[AnyContent] =
    (authenticate andThen getData andThen requireData).async {
      implicit request =>
        viewmodel(index, mode).retrieve.right.map {
          vm =>
            post(PreviousPostCodeLookupId(index), vm, mode)
        }
    }
}
