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

package forms.behaviours

import forms.FormSpec
import forms.mappings.{Constraints, PayeMapping, PayeMappingString}
import play.api.data.{Form, FormError}
import wolfendale.scalacheck.regexp.RegexpGen

trait PayeStringBehaviours extends FormSpec with StringFieldBehaviours with Constraints with PayeMappingString {

  def formWithPayeField(
                         form: Form[_],
                         fieldName: String,
                         keyPayeLength: String,
                         keyPayeInvalid: String): Unit = {

    "behave like a form with a paye field" should {
      behave like fieldThatBindsValidData(
        form,
        fieldName,
        RegexpGen.from(regexPaye)
      )

      behave like fieldWithMaxLength(
        form,
        fieldName,
        maxLength = PayeMapping.maxPayeLength,
        lengthError = FormError(fieldName, keyPayeLength, Seq(PayeMapping.maxPayeLength))
      )

      behave like fieldWithRegex(
        form,
        fieldName,
        "A1_",
        FormError(fieldName, keyPayeInvalid, Seq(regexPaye))
      )
    }
  }

}
