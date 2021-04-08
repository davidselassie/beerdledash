import scalatags.Text.all._
import scalatags.Text.tags2.title

object HtmlRenderer {

  def htmlContent(bodyContent: Frag, headExtras: Frag = frag()): doctype =
    doctype("html")(
      html(lang := "en")(
        head(
          meta(
            name := "viewport",
            content := "width=device-width, initial-scale=1"
          ),
          link(rel := "stylesheet", href := "static/site.css"),
          link(rel := "icon", href := "static/favicon.png"),
          title("Beerdledash"),
          headExtras
        ),
        body(bodyContent)
      )
    )
}
