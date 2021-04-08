import scalatags.Text.all._

object TurboTags {

  def turboScript(eventSource: String): Frag = script(`type` := "module")(
    raw(
      """import * as Turbo from 'https://cdn.skypack.dev/pin/@hotwired/turbo@v7.0.0-beta.4-TQFv5Y2xd4hn2VnTxVul/mode=imports/optimized/@hotwired/turbo.js'; """
    ),
    raw(
      s"""Turbo.connectStreamSource(new EventSource(""""
    ),
    raw(
      eventSource
    ), // Query string, if any will be escaped by .queryString().
    raw(""""));""")
  )

  val turboFrame: ConcreteHtmlTag[String] = tag("turbo-frame")

  val turboStream: ConcreteHtmlTag[String] = tag("turbo-stream")

  val template: ConcreteHtmlTag[String] = tag("template")
}
