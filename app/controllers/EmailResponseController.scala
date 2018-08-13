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

import controllers.model.EmailEvents
import javax.inject.Inject
import play.api.libs.json.JsValue
import play.api.mvc.{Action, BodyParsers}
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

class EmailResponseController @Inject()(

                                       ) extends FrontendController {

  def post(id: String): Action[JsValue] = Action(BodyParsers.parse.tolerantJson) {
    implicit request =>
      request.body.validate[EmailEvents].fold(
        invalid => BadRequest,
        valid => Ok
      )
  }

}