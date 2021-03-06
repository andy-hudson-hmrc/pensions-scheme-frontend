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
import controllers.actions._
import forms.register.trustees.partnership.PartnershipUniqueTaxReferenceFormProvider
import identifiers.register.trustees.partnership.{PartnershipDetailsId, PartnershipUniqueTaxReferenceId}
import javax.inject.Inject
import models.{Index, Mode, UniqueTaxReference}
import play.api.data.Form
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent}
import uk.gov.hmrc.play.bootstrap.controller.FrontendController
import utils.annotations.TrusteesPartnership
import utils.{Enumerable, Navigator, UserAnswers}
import views.html.register.trustees.partnership.partnershipUniqueTaxReference

import scala.concurrent.Future

class PartnershipUniqueTaxReferenceController @Inject()(
                                                         appConfig: FrontendAppConfig,
                                                         override val messagesApi: MessagesApi,
                                                         dataCacheConnector: DataCacheConnector,
                                                         authenticate: AuthAction,
                                                         @TrusteesPartnership navigator: Navigator,
                                                         getData: DataRetrievalAction,
                                                         requireData: DataRequiredAction,
                                                         formProvider: PartnershipUniqueTaxReferenceFormProvider
                                                       ) extends FrontendController with Retrievals with I18nSupport with Enumerable.Implicits {

  private val form: Form[UniqueTaxReference] = formProvider()

  def onPageLoad(mode: Mode, index: Index): Action[AnyContent] = (authenticate andThen getData andThen requireData).async {
    implicit request =>
      PartnershipDetailsId(index).retrieve.right.map { details =>
        val redirectResult = request.userAnswers.get(PartnershipUniqueTaxReferenceId(index)) match {
          case None =>
            Ok(partnershipUniqueTaxReference(appConfig, form, mode, index, details.name))
          case Some(value) =>
            Ok(partnershipUniqueTaxReference(appConfig, form.fill(value), mode, index, details.name))
        }
        Future.successful(redirectResult)
      }
  }

  def onSubmit(mode: Mode, index: Index): Action[AnyContent] = (authenticate andThen getData andThen requireData).async {
    implicit request =>
      PartnershipDetailsId(index).retrieve.right.map { details =>
        form.bindFromRequest().fold(
          (formWithErrors: Form[_]) =>
            Future.successful(BadRequest(partnershipUniqueTaxReference(appConfig, formWithErrors, mode, index, details.name))),
          value =>
            dataCacheConnector.save(
              request.externalId,
              PartnershipUniqueTaxReferenceId(index),
              value
            ).map {
              json =>
                Redirect(navigator.nextPage(PartnershipUniqueTaxReferenceId(index), mode, UserAnswers(json)))
            }
        )
      }
  }
}
