package forms.$routeFile$

import javax.inject.Inject
import forms.mappings.Mappings
import play.api.data.Form
import models.$routeFile$.$className$

class $className$FormProvider @Inject() extends Mappings {

  def apply(): Form[$className$] =
    Form(
      "value" -> enumerable[$className$]("$className;format="decap"$.error.required")
    )
}
