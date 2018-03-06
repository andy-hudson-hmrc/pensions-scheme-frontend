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

import javax.inject.Inject

import config.FrontendAppConfig
import connectors.DataCacheConnector
import controllers.Retrievals
import controllers.actions._
import forms.register.establishers.individual.AddressListFormProvider
import identifiers.register._
import models.Mode
import models.requests.DataRequest
import play.api.data.Form
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent, Result}
import uk.gov.hmrc.play.bootstrap.controller.FrontendController
import utils.{Enumerable, Navigator, UserAnswers}
import views.html.register.insurerAddressList

import scala.concurrent.Future

class InsurerAddressListController @Inject()(
                                       appConfig: FrontendAppConfig,
                                       override val messagesApi: MessagesApi,
                                       dataCacheConnector: DataCacheConnector,
                                       navigator: Navigator,
                                       authenticate: AuthAction,
                                       getData: DataRetrievalAction,
                                       requireData: DataRequiredAction,
                                       formProvider: AddressListFormProvider
                                     ) extends FrontendController with Retrievals with I18nSupport with Enumerable.Implicits {


  def onPageLoad(mode: Mode): Action[AnyContent] = (authenticate andThen getData andThen requireData).async {
    implicit request =>
      retrieveSchemeName { schemeName =>

        request.userAnswers.get(InsurerPostCodeLookupId) match {
          case None =>
           Future.successful(Redirect(controllers.register.routes.InsurerPostCodeLookupController.onPageLoad(mode)))
          case Some(addresses) =>
            Future.successful(Ok(insurerAddressList(appConfig, formProvider(addresses), mode, schemeName, addresses)))
        }

      }
  }

  def onSubmit(mode: Mode): Action[AnyContent] = (authenticate andThen getData andThen requireData).async {
    implicit request =>
      retrieveSchemeName { schemeName =>
        request.userAnswers.get(InsurerPostCodeLookupId) match {
          case None =>
            Future.successful(Redirect(controllers.register.routes.InsurerPostCodeLookupController.onPageLoad(mode)))
          case Some(addresses) =>
            formProvider(addresses).bindFromRequest().fold(
              (formWithErrors: Form[_]) =>
                Future.successful(BadRequest(insurerAddressList(appConfig, formWithErrors, mode, schemeName, addresses))),
              (value) =>
                dataCacheConnector.save(
                  request.externalId,
                  InsurerAddressId,
                  addresses(value).copy(country = "GB")
                ).map(cacheMap =>
                  Redirect(navigator.nextPage(InsurerAddressListId, mode)(new UserAnswers(cacheMap))))
            )
        }
      }
  }

}