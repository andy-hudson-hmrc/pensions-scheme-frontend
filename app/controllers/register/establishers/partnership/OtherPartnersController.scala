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

package controllers.register.establishers.partnership

import config.FrontendAppConfig
import connectors.DataCacheConnector
import controllers.Retrievals
import controllers.actions._
import forms.register.establishers.partnership.OtherPartnersFormProvider
import identifiers.register.establishers.partnership.OtherPartnersId
import javax.inject.Inject
import models.{Index, Mode}
import play.api.data.Form
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent}
import uk.gov.hmrc.play.bootstrap.controller.FrontendController
import utils.annotations.EstablisherPartnership
import utils.{Navigator, UserAnswers}
import views.html.register.establishers.partnership.otherPartners

import scala.concurrent.Future

class OtherPartnersController @Inject()(
                                         appConfig: FrontendAppConfig,
                                         override val messagesApi: MessagesApi,
                                         dataCacheConnector: DataCacheConnector,
                                         @EstablisherPartnership navigator: Navigator,
                                         authenticate: AuthAction,
                                         getData: DataRetrievalAction,
                                         requireData: DataRequiredAction,
                                         formProvider: OtherPartnersFormProvider
                                       ) extends FrontendController with Retrievals with I18nSupport {

  private val form: Form[Boolean] = formProvider()

  def onPageLoad(mode: Mode, establisherIndex: Index): Action[AnyContent] = (authenticate andThen getData andThen requireData).async {
    implicit request =>
      retrievePartnershipName(establisherIndex) { partnershipName =>
        val redirectResult = request.userAnswers.get(OtherPartnersId(establisherIndex)) match {
          case None => Ok(otherPartners(appConfig, form, mode, establisherIndex, partnershipName))
          case Some(value) => Ok(otherPartners(appConfig, form.fill(value), mode, establisherIndex, partnershipName))
        }
        Future.successful(redirectResult)
      }

  }

  def onSubmit(mode: Mode, establisherIndex: Index): Action[AnyContent] = (authenticate andThen getData andThen requireData).async {
    implicit request =>
      retrievePartnershipName(establisherIndex) {
        partnershipName =>
          form.bindFromRequest().fold(
            (formWithErrors: Form[_]) =>
              Future.successful(BadRequest(otherPartners(appConfig, formWithErrors, mode, establisherIndex, partnershipName))),
            value =>
              dataCacheConnector.save(request.externalId, OtherPartnersId(establisherIndex), value).map(cacheMap =>
                Redirect(navigator.nextPage(OtherPartnersId(establisherIndex), mode, UserAnswers(cacheMap))))
          )
      }
  }
}
