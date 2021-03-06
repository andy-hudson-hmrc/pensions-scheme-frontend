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

package controllers

import config.FrontendAppConfig
import connectors.DataCacheConnector
import controllers.actions._
import forms.DeleteSchemeFormProvider
import identifiers.register.SchemeDetailsId
import javax.inject.Inject
import play.api.data.Form
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent}
import uk.gov.hmrc.play.bootstrap.controller.FrontendController
import views.html.deleteScheme

import scala.concurrent.Future

class DeleteSchemeController @Inject()(
                                        appConfig: FrontendAppConfig,
                                        override val messagesApi: MessagesApi,
                                        dataCacheConnector: DataCacheConnector,
                                        authenticate: AuthAction,
                                        getData: DataRetrievalAction,
                                        requireData: DataRequiredAction,
                                        formProvider: DeleteSchemeFormProvider
                                      ) extends FrontendController with I18nSupport with Retrievals {

  private val form: Form[Boolean] = formProvider()

  def onPageLoad: Action[AnyContent] = (authenticate andThen getData).async {
    implicit request =>
      request.userAnswers match {
        case Some(data) => {
          data.get(SchemeDetailsId) match {
            case Some(schemeDetails) =>
              Future.successful(Ok(deleteScheme(appConfig, form, schemeDetails.schemeName)))
            case None => Future.successful(Redirect(controllers.routes.SessionExpiredController.onPageLoad()))
          }
        }
        case None => Future.successful(Redirect(controllers.routes.SchemesOverviewController.onPageLoad()))
      }
  }

  def onSubmit: Action[AnyContent] = (authenticate andThen getData andThen requireData).async {
    implicit request =>
      retrieveSchemeName { schemeName =>
        form.bindFromRequest().fold(
          (formWithErrors: Form[_]) =>
            Future.successful(BadRequest(deleteScheme(appConfig, formWithErrors, schemeName))),
          {
            case true =>
              dataCacheConnector.removeAll(request.externalId).map { _ =>
                Redirect(controllers.routes.WhatYouWillNeedController.onPageLoad())
              }
            case false =>
              Future.successful(Redirect(controllers.routes.SchemesOverviewController.onPageLoad()))
          }
        )
      }
  }
}
