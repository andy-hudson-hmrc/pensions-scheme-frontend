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

package identifiers.register.establishers.partnership.partner

import identifiers._
import identifiers.register.establishers.EstablishersId
import models.AddressYears
import play.api.libs.json.{JsPath, JsResult}
import utils.UserAnswers
import utils.checkyouranswers.{AddressYearsCYA, CheckYourAnswers}

case class PartnerAddressYearsId(establisherIndex: Int, partnerIndex: Int) extends TypedIdentifier[AddressYears] {

  override def path: JsPath = EstablishersId(establisherIndex).path \ "partner" \ partnerIndex \ PartnerAddressYearsId.toString

  override def cleanup(value: Option[AddressYears], userAnswers: UserAnswers): JsResult[UserAnswers] = {
    value match {
      case Some(AddressYears.OverAYear) =>
        userAnswers.remove(PartnerPreviousAddressPostcodeLookupId(establisherIndex, partnerIndex))
          .flatMap(_.remove(PartnerPreviousAddressId(establisherIndex, partnerIndex)))
          .flatMap(_.remove(PartnerPreviousAddressListId(establisherIndex, partnerIndex)))
      case _ => super.cleanup(value, userAnswers)
    }
  }
}

object PartnerAddressYearsId {
  override lazy val toString: String = "partnerAddressYears"

  implicit val cya: CheckYourAnswers[PartnerAddressYearsId] =
    AddressYearsCYA[PartnerAddressYearsId]("messages__partner_address_years_question_cya_label")()
}

