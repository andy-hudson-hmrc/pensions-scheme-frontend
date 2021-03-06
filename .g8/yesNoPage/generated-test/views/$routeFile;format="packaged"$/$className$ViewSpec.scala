package views.$routeFile$

import play.api.data.Form
import controllers.$routeFile$.routes
import forms.$routeFile$.$className$FormProvider
import views.behaviours.YesNoViewBehaviours
import models.NormalMode
import views.html.$routeFile$.$className;format="decap"$

class $className$ViewSpec extends YesNoViewBehaviours {

  val messageKeyPrefix = "$className;format="decap"$"

  val form = new $className$FormProvider()()

  def createView = () => $className;format="decap"$(frontendAppConfig, form, NormalMode)(fakeRequest, messages)

  def createViewUsingForm = (form: Form[_]) => $className;format="decap"$(frontendAppConfig, form, NormalMode)(fakeRequest, messages)

  "$className$ view" must {

    behave like normalPage(createView, messageKeyPrefix, messages("messages__$className;format="decap"$__heading"))

    behave like pageWithBackLink(createView)

    behave like yesNoPage(createViewUsingForm, messageKeyPrefix, routes.$className$Controller.onSubmit(NormalMode).url)
  }
}
