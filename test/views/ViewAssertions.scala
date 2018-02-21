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

package views

import org.jsoup.nodes.Document
import org.scalatest.matchers.{MatchResult, Matcher}
import scala.collection.JavaConverters._

trait ViewAssertions {

  def haveContent(content: String): Matcher[Document] =
    Matcher {
      doc =>
        MatchResult(
          doc.text().contains(content),
          rawFailureMessage = s"page content does not contain: '$content'",
          rawNegatedFailureMessage = s"page content contains: '$content'"
        )
    }

  def haveBrowserTitle(title: String): Matcher[Document] =
    Matcher {
      doc =>
        MatchResult(
          doc.title.contains(title),
          rawFailureMessage = s"page title: '${doc.title}', does not equal '$title'",
          rawNegatedFailureMessage = s"page title equals '$title'"
        )
    }

  def haveHeading(header: String, level: Int = 1): Matcher[Document] =
    Matcher {
      doc =>
        Option(doc.select(s"h$level").first)
          .toRight(MatchResult(
              matches = false,
              s"no h$level found",
              s"found h$level"
          ))
          .right.map {
            element =>
              MatchResult(
                element.text() == header,
                s"h$level content: '${element.text()}' was not: '$header'",
                s"h$level has content: '$header'"
              )
          }.merge
    }

  def haveSubHeading(subHeader: String): Matcher[Document] =
    Matcher {
      doc =>
        MatchResult(
          Option(doc.select("header > h1 + .heading-secondary").first)
            .map(_.text())
            .contains(subHeader),
          rawFailureMessage = s"no subheader found matching $subHeader",
          rawNegatedFailureMessage = s"subheader found matching $subHeader"
        )
    }

  def haveLink(links: (String, String)*): Matcher[Document] = {
    links.map {
      case (text, url) =>
        Matcher {
          doc: Document =>
            MatchResult(
              Option(doc.select(s"a[href=$url]").first)
                .map(_.text())
                .contains(text),
              rawFailureMessage = s"no url found matching $text",
              rawNegatedFailureMessage = s"url found matching $text"
            )
        }
    }.reduce { _ and _ }
  }

  def haveButton(buttonText: String): Matcher[Document] =
    Matcher {
      doc =>
        MatchResult(
          doc.select("button, select[type=submit]").asScala
            .map(_.text())
            .contains(buttonText),
          rawFailureMessage = s"no button found with $buttonText text",
          rawNegatedFailureMessage = s"button found with $buttonText text"
        )
    }

  def haveAnErrorSummary: Matcher[Document] =
    Matcher {
      doc =>
        MatchResult(
          Option(doc.select(".error-summary").first).isDefined,
          "no error summary was found",
          "an error summary was found"
        )
    }

  def haveRadioButton(labelText: String, value: String): Matcher[Document] =
    Matcher {
      doc =>

        val matches = for {
          label      <- doc.select("label").asScala
                          .find(_.text() == labelText)
          id         <- noneIfEmpty(label.attr("for"))
          radio      <- Option(doc.getElementById(id))
          kind       <- noneIfEmpty(radio.attr("type"))
          radioValue <- noneIfEmpty(radio.attr("value"))
        } yield radioValue == value && kind == "radio"

        MatchResult(
          matches.getOrElse(false),
          rawFailureMessage = s"no radio button found with label: '$labelText' and value: '$value",
          rawNegatedFailureMessage = s"found a radio button with label: '$labelText' and value: '$value"
        )
    }

  def haveFieldset(legendText: String): Matcher[Document] =
    Matcher {
      doc =>

        val matches = doc.select("fieldset > legend").asScala
          .exists(_.text() == legendText)

        MatchResult(
          matches,
          rawFailureMessage = s"no fieldset found with legend: $legendText",
          rawNegatedFailureMessage = s"fieldset found with legend: $legendText"
        )
    }

  def haveATextField(labelText: String, value: Option[String] = None): Matcher[Document] =
    Matcher {
      doc =>

        val matches = for {
          label      <- doc.select("label").asScala
                          .find(_.text() == labelText)
          id         <- noneIfEmpty(label.attr("for"))
          field      <- Option(doc.getElementById(id))
          kind       <- noneIfEmpty(field.attr("type"))
          fieldValue <- noneIfEmpty(field.attr("value"))
        } yield value.forall(_ == fieldValue) && kind == "text"

        MatchResult(
          matches.getOrElse(false),
          rawFailureMessage = s"no text field found with label: '$labelText' and value: '$value'",
          rawNegatedFailureMessage = s"found a field with label: '$labelText' and value: '$value'"
        )
    }

  def haveATextArea(labelText: String, value: Option[String] = None): Matcher[Document] =
    Matcher {
      doc =>

        val matches = for {
          label <- doc.select("label").asScala
            .find(_.text() == labelText)
          id <- noneIfEmpty(label.attr("for"))
          textarea <- Option(doc.getElementById(id))
          fieldValue <- noneIfEmpty(textarea.attr("value"))
        } yield value.forall(_ == fieldValue)

        MatchResult(
          matches.getOrElse(false),
          rawFailureMessage = s"no textarea found with label: '$labelText' and value: '$value'",
          rawNegatedFailureMessage = s"found a textarea with label: '$labelText' and value: '$value'"
        )
    }

  private def noneIfEmpty(str: String): Option[String] =
    str match {
      case "" =>
        None
      case v  =>
        Some(v)
    }
}


