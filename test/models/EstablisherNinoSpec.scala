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

package models

import org.scalatest.{MustMatchers, OptionValues, WordSpecLike}
import play.api.libs.json.{JsError, JsPath, Json}
import reactivemongo.play.json.ValidationError

class EstablisherNinoSpec extends WordSpecLike with MustMatchers with OptionValues {

  "Reads" must {
    "successfully read true" in {
      val json = Json.obj("hasNino" -> true, "nino" -> "AB020202A")

      Json.fromJson[Nino](json).asOpt.value mustEqual Nino.Yes("AB020202A")
    }

    "successfully read false" in {
      val json = Json.obj("hasNino" -> false, "reason" -> "haven't got Nino")

      Json.fromJson[Nino](json).asOpt.value mustEqual Nino.No("haven't got Nino")
    }

    "return failure for true without nino" in {
      val json = Json.obj("hasNino" -> true)

      Json.fromJson[Nino](json) mustEqual JsError("NINO Value expected")
    }

    "return failure for false without reason" in {
      val json = Json.obj("hasNino" -> false)

      Json.fromJson[Nino](json) mustEqual JsError("Reason expected")
    }

    "return failure for when no input given" in {
      val json = Json.obj("hasNino" -> "notABoolean")


      Json.fromJson[Nino](json) mustEqual JsError(Seq((JsPath \ "hasNino", Seq(ValidationError(Seq("error.expected.jsboolean"), Seq())))))
    }
  }

  "Writes" must {
    "return successfully write Yes" in {
      Json.toJson[Nino](Nino.Yes("AB020202A")) mustEqual Json.obj("hasNino" -> true, "nino" -> "AB020202A")
    }
    "return successfully write No" in {
      Json.toJson[Nino](Nino.No("haven't got Nino")) mustEqual Json.obj("hasNino" -> false, "reason" -> "haven't got Nino")
    }
  }
}
