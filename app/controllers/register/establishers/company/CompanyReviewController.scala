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

package controllers.register.establishers.company

import config.FrontendAppConfig
import controllers.Retrievals
import controllers.actions._
import identifiers.register.SchemeDetailsId
import identifiers.register.establishers.IsEstablisherCompleteId
import identifiers.register.establishers.company.{CompanyDetailsId, CompanyReviewId, IsCompanyCompleteId}
import javax.inject.Inject
import models.{Index, NormalMode}
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent}
import uk.gov.hmrc.play.bootstrap.controller.FrontendController
import utils.annotations.EstablishersCompany
import utils.{Navigator, SectionComplete}
import views.html.register.establishers.company.companyReview

import scala.concurrent.Future

class CompanyReviewController @Inject()(appConfig: FrontendAppConfig,
                                        override val messagesApi: MessagesApi,
                                        @EstablishersCompany navigator: Navigator,
                                        authenticate: AuthAction,
                                        getData: DataRetrievalAction,
                                        requireData: DataRequiredAction,
                                        sectionComplete: SectionComplete) extends FrontendController with I18nSupport with Retrievals {

  def onPageLoad(index: Index): Action[AnyContent] = (authenticate andThen getData andThen requireData).async {
    implicit request =>
      (SchemeDetailsId and CompanyDetailsId(index)).retrieve.right.map {
        case schemeDetails ~ companyDetails =>
          val directors: Seq[String] = request.userAnswers.allDirectorsAfterDelete(index).map(_.name)

          Future.successful(Ok(companyReview(appConfig, index, schemeDetails.schemeName, companyDetails.companyName, directors)))
      }
  }

  def onSubmit(index: Index): Action[AnyContent] = (authenticate andThen getData andThen requireData).async {
    implicit request =>
      val allDirectors = request.userAnswers.allDirectorsAfterDelete(index)
      val allDirectorsCompleted = allDirectors.nonEmpty & (allDirectors.count(!_.isCompleted) == 0)

      val isCompanyComplete = request.userAnswers.get(IsCompanyCompleteId(index)).getOrElse(false)

      if (allDirectorsCompleted & isCompanyComplete) {
        sectionComplete.setCompleteFlag(IsEstablisherCompleteId(index), request.userAnswers, value = true).map { _ =>
          Redirect(navigator.nextPage(CompanyReviewId(index), NormalMode, request.userAnswers))
        }
      }
      else {
        Future.successful(Redirect(navigator.nextPage(CompanyReviewId(index), NormalMode, request.userAnswers)))
      }
  }

}
