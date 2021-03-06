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

package identifiers.register.trustees.company

import identifiers._
import identifiers.register.trustees.TrusteesId
import models.address.Address
import play.api.libs.json._
import utils.CountryOptions
import utils.checkyouranswers.{AddressCYA, CheckYourAnswers}

case class CompanyPreviousAddressId(index: Int) extends TypedIdentifier[Address] {
  override def path: JsPath = TrusteesId(index).path \ CompanyPreviousAddressId.toString
}

object CompanyPreviousAddressId {
  override def toString: String = "companyPreviousAddress"

  implicit def cya(implicit countryOptions: CountryOptions): CheckYourAnswers[CompanyPreviousAddressId] =
    AddressCYA("messages__common__cya__previous_address")()
}