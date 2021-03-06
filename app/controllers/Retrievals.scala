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

import identifiers.TypedIdentifier
import identifiers.register.SchemeDetailsId
import identifiers.register.establishers.company.CompanyDetailsId
import identifiers.register.establishers.individual.EstablisherDetailsId
import identifiers.register.establishers.partnership.PartnershipDetailsId
import models.person.PersonDetails
import models.register.SchemeDetails
import models.requests.DataRequest
import models.{CompanyDetails, PartnershipDetails}
import play.api.libs.json.Reads
import play.api.mvc.{AnyContent, Result}
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.Future
import scala.language.implicitConversions

trait Retrievals {

  this: FrontendController =>

  private[controllers] def retrieveCompanyName(index: Int)
                                              (f: String => Future[Result])
                                              (implicit request: DataRequest[AnyContent]): Future[Result] = {
    retrieve[CompanyDetails](CompanyDetailsId(index)) { companyDetails =>
      f(companyDetails.companyName)
    }
  }

  private[controllers] def retrievePartnershipName(index: Int)
                                                  (f: String => Future[Result])
                                                  (implicit request: DataRequest[AnyContent]): Future[Result] = {
    retrieve[PartnershipDetails](PartnershipDetailsId(index)) { partnershipDetails =>
      f(partnershipDetails.name)
    }
  }

  private[controllers] def retrieveSchemeName(f: String => Future[Result])
                                             (implicit request: DataRequest[AnyContent]): Future[Result] = {
    retrieve[SchemeDetails](SchemeDetailsId) { schemeDetails =>
      f(schemeDetails.schemeName)
    }
  }

  private[controllers] def retrieveEstablisherName(index: Int)
                                                  (f: String => Future[Result])
                                                  (implicit request: DataRequest[AnyContent]): Future[Result] = {
    retrieve[PersonDetails](EstablisherDetailsId(index)) { establisherDetails =>
      f(establisherDetails.fullName)
    }
  }

  private[controllers] def retrieve[A](id: TypedIdentifier[A])
                                      (f: (A) => Future[Result])
                                      (implicit request: DataRequest[AnyContent], r: Reads[A]): Future[Result] = {
    request.userAnswers.get(id).map(f).getOrElse {
      Future.successful(Redirect(controllers.routes.SessionExpiredController.onPageLoad()))
    }

  }

  case class ~[A, B](a: A, b: B)

  trait Retrieval[A] {
    self =>

    def retrieve(implicit request: DataRequest[AnyContent]): Either[Future[Result], A]

    def and[B](query: Retrieval[B]): Retrieval[A ~ B] =
      new Retrieval[A ~ B] {
        override def retrieve(implicit request: DataRequest[AnyContent]): Either[Future[Result], A ~ B] = {
          for {
            a <- self.retrieve.right
            b <- query.retrieve.right
          } yield new ~(a, b)
        }
      }
  }

  object Retrieval {

    def apply[A](f: DataRequest[AnyContent] => Either[Future[Result], A]): Retrieval[A] =
      new Retrieval[A] {
        override def retrieve(implicit request: DataRequest[AnyContent]): Either[Future[Result], A] =
          f(request)
      }

    def static[A](a: A): Retrieval[A] =
      Retrieval {
        implicit request =>
          Right(a)
      }
  }

  implicit def fromId[A](id: TypedIdentifier[A])(implicit rds: Reads[A]): Retrieval[A] =
    Retrieval {
      implicit request =>
        request.userAnswers.get(id) match {
          case Some(value) => Right(value)
          case None => Left(Future.successful(Redirect(controllers.routes.SessionExpiredController.onPageLoad())))
        }
    }

  implicit def merge(f: Either[Future[Result], Future[Result]]): Future[Result] =
    f.merge
}
