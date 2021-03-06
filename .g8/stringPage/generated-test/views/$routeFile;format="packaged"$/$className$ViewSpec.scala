package views.$routeFile$

import play.api.data.Form
import controllers.$routeFile$.routes
import forms.$routeFile$.$className$FormProvider
import models.NormalMode
import views.behaviours.StringViewBehaviours
import views.html.$routeFile$.$className;format="decap"$

class $className$ViewSpec extends StringViewBehaviours {

  val messageKeyPrefix = "$className;format="decap"$"

  val form = new $className$FormProvider()()

  def createView = () => $className;format="decap"$(frontendAppConfig, form, NormalMode)(fakeRequest, messages)

  def createViewUsingForm = (form: Form[String]) => $className;format="decap"$(frontendAppConfig, form, NormalMode)(fakeRequest, messages)

  "$className$ view" must {
    behave like normalPage(createView, messageKeyPrefix, messages(s"messages__\${messageKeyPrefix}__heading"))

    behave like pageWithBackLink(createView)

    behave like stringPage(createViewUsingForm, messageKeyPrefix, controllers.$routeFile$.routes.$className$Controller.onSubmit(NormalMode).url)
  }
}
