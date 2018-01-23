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

package utils

import identifiers.register._
import identifiers.register.establishers._
import identifiers.register.establishers.individual._
import uk.gov.hmrc.http.cache.client.CacheMap
import models._
import controllers.register.establishers.routes
import models.register._
import models.register.establishers.EstablisherKind
import models.register.establishers.individual._
import identifiers.register.establishers.company._

import scala.util.{Success, Try}

class UserAnswers(val cacheMap: CacheMap) extends Enumerable.Implicits with MapFormats {

  def companyDetails: Option[EstablishersIndividualMap[CompanyDetails]] = cacheMap.getEntry[EstablishersIndividualMap[CompanyDetails]](
    CompanyDetailsId.toString)

  def companyDetails(index: Int): Try[Option[CompanyDetails]] = companyDetails.map(_.get(index)).getOrElse(Success(None))

  def contactDetails: Option[EstablishersIndividualMap[ContactDetails]] =
    cacheMap.getEntry[EstablishersIndividualMap[ContactDetails]](ContactDetailsId.toString)

  def contactDetails(index: Int): Try[Option[ContactDetails]] = contactDetails.map(_.get(index)).getOrElse(Success(None))

  def establisherNino: Option[EstablishersIndividualMap[EstablisherNino]] =
    cacheMap.getEntry[EstablishersIndividualMap[EstablisherNino]](EstablisherNinoId.toString)

  def establisherNino(index:Int): Try[Option[EstablisherNino]] = establisherNino.map(_.get(index)).getOrElse(Success(None))

  def companyAddressYears: Option[EstablishersIndividualMap[models.register.establishers.company.CompanyAddressYears]] =
    cacheMap.getEntry[EstablishersIndividualMap[models.register.establishers.company.CompanyAddressYears]](CompanyAddressYearsId.toString)

  def companyAddressYears(index: Int): Try[Option[models.register.establishers.company.CompanyAddressYears]] =
    companyAddressYears.map(_.get(index)).getOrElse(Success(None))

  def uniqueTaxReference: Option[EstablishersIndividualMap[UniqueTaxReference]] =
    cacheMap.getEntry[EstablishersIndividualMap[UniqueTaxReference]](UniqueTaxReferenceId.toString)

  def uniqueTaxReference(index: Int): Try[Option[UniqueTaxReference]] = uniqueTaxReference.map(_.get(index)).getOrElse(
    Success(None))

  def addEstablisher(): Option[Boolean] = cacheMap.getEntry[Boolean](AddEstablisherId.toString)

  def establisherKind(index: Int): Try[Option[EstablisherKind]] = establisherKind.map(_.get(index)).getOrElse(Success(None))

  def establisherKind: Option[EstablishersIndividualMap[EstablisherKind]] = cacheMap.getEntry[EstablishersIndividualMap[EstablisherKind]](
    EstablisherKindId.toString)

  def establisherDetails: Option[EstablishersIndividualMap[EstablisherDetails]] =
    cacheMap.getEntry[EstablishersIndividualMap[EstablisherDetails]](EstablisherDetailsId.toString)

  def establisherDetails(index: Int): Try[Option[EstablisherDetails]] = establisherDetails.map(_.get(index)).getOrElse(
    Success(None))

  def allEstablishers: Option[Map[String, String]] = {
    for {
      individualEst <- establisherDetails.map(_.getValues.map(estDetails =>
        (estDetails.establisherName, routes.AddEstablisherController.onPageLoad(NormalMode).url)
      ))
      companyEst <- companyDetails.map(_.getValues.map(details =>
        (details.companyName, routes.AddEstablisherController.onPageLoad(NormalMode).url)
      ))
    } yield (individualEst ++ companyEst).toMap
  }

  def schemeEstablishedCountry: Option[String] = cacheMap.getEntry[String](SchemeEstablishedCountryId.toString)

  def addressYears: Option[EstablishersIndividualMap[AddressYears]] =
    cacheMap.getEntry[EstablishersIndividualMap[AddressYears]](AddressYearsId.toString)

  def addressYears(index: Int): Try[Option[AddressYears]] = addressYears.map(_.get(index)).getOrElse(Success(None))

  def uKBankAccount: Option[Boolean] = cacheMap.getEntry[Boolean](UKBankAccountId.toString)

  def uKBankDetails: Option[UKBankDetails] = cacheMap.getEntry[UKBankDetails](UKBankDetailsId.toString)

  def benefits: Option[Benefits] = cacheMap.getEntry[Benefits](BenefitsId.toString)

  def benefitsInsurer: Option[BenefitsInsurer] = cacheMap.getEntry[BenefitsInsurer](BenefitsInsurerId.toString)

  def membership: Option[Membership] = cacheMap.getEntry[Membership](MembershipId.toString)

  def membershipFuture: Option[MembershipFuture] = cacheMap.getEntry[MembershipFuture](MembershipFutureId.toString)

  def investmentRegulated: Option[Boolean] = cacheMap.getEntry[Boolean](InvestmentRegulatedId.toString)

  def securedBenefits: Option[Boolean] = cacheMap.getEntry[Boolean](SecuredBenefitsId.toString)

  def occupationalPensionScheme: Option[Boolean] = cacheMap.getEntry[Boolean](OccupationalPensionSchemeId.toString)

  def schemeDetails: Option[SchemeDetails] = cacheMap.getEntry[SchemeDetails](SchemeDetailsId.toString)
}