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

package controllers.register.trustees

import config.FrontendAppConfig
import connectors.DataCacheConnector
import controllers.Retrievals
import controllers.actions._
import forms.register.trustees.TrusteeKindFormProvider
import identifiers.register.SchemeDetailsId
import identifiers.register.trustees.TrusteeKindId
import javax.inject.Inject
import models.{Index, Mode}
import play.api.Logger
import play.api.data.Form
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.libs.json.JsResultException
import play.api.mvc.{Action, AnyContent}
import uk.gov.hmrc.play.bootstrap.controller.FrontendController
import utils.annotations.Trustees
import utils.{Enumerable, Navigator}
import views.html.register.trustees.trusteeKind

import scala.concurrent.Future

class TrusteeKindController @Inject()(
                                       appConfig: FrontendAppConfig,
                                       override val messagesApi: MessagesApi,
                                       dataCacheConnector: DataCacheConnector,
                                       @Trustees navigator: Navigator,
                                       authenticate: AuthAction,
                                       getData: DataRetrievalAction,
                                       requireData: DataRequiredAction,
                                       formProvider: TrusteeKindFormProvider
                                     ) extends FrontendController with Retrievals with I18nSupport with Enumerable.Implicits {

  private val form = formProvider()

  def onPageLoad(mode: Mode, index: Index): Action[AnyContent] = (authenticate andThen getData andThen requireData).async {
    implicit request =>
      SchemeDetailsId.retrieve.right.map { schemeDetails =>
        val preparedForm = request.userAnswers.get(TrusteeKindId(index)) match {
          case None => form
          case Some(value) => form.fill(value)
        }
        Future.successful(Ok(trusteeKind(appConfig, preparedForm, mode, index, schemeDetails.schemeName)))
      }
  }

  def onSubmit(mode: Mode, index: Index): Action[AnyContent] = (authenticate andThen getData andThen requireData).async {
    implicit request =>
      SchemeDetailsId.retrieve.right.map { schemeDetails =>
        form.bindFromRequest().fold(
          (formWithErrors: Form[_]) =>
            Future.successful(BadRequest(trusteeKind(appConfig, formWithErrors, mode, index, schemeDetails.schemeName))),
          value =>
            request.userAnswers.set(TrusteeKindId(index))(value).fold(
              errors => {
                Logger.error("Unable to set user answer", JsResultException(errors))
                Future.successful(InternalServerError)
              },
              userAnswers =>
                Future.successful(Redirect(navigator.nextPage(TrusteeKindId(index), mode, userAnswers)))
            )
        )
      }
  }
}
