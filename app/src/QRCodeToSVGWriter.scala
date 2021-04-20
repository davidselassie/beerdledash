import akka.http.scaladsl.marshalling.{Marshaller, ToEntityMarshaller}
import akka.http.scaladsl.model.{HttpEntity, MediaType}
import com.google.zxing.qrcode.encoder.QRCode
import scalatags.Text.TypedTag
import scalatags.Text.implicits._
import scalatags.Text.svgAttrs.{
  height,
  shapeRendering,
  viewBox,
  width,
  x,
  xmlns,
  y
}
import scalatags.Text.svgTags.{rect, svg}
import scalatags.generic.Namespace

object QRCodeToSVGWriter {

  private val quietZone = 4

  final case class Svg(tag: TypedTag[String]) extends AnyVal

  def render(qr: QRCode): Svg = {
    val mat = qr.getMatrix

    val rects =
      for (
        moduleX <- Range(0, mat.getHeight);
        moduleY <- Range(0, mat.getWidth) if mat.get(moduleX, moduleY) > 0
      ) yield {
        rect(
          x := moduleX,
          y := moduleY,
          width := 1,
          height := 1
        )
      }

    Svg(
      svg(
        xmlns := Namespace.svgNamespaceConfig.uri,
        viewBox := s"${-quietZone} ${-quietZone} ${mat.getWidth + quietZone * 2} ${mat.getHeight + quietZone * 2}",
        shapeRendering := "crispEdges"
      )(rects: _*)
    )
  }

  val `image/svg+xml`: MediaType.WithOpenCharset =
    MediaType.customWithOpenCharset("image", "svg+xml", List("svg"))

  implicit val SvgMarshaller: ToEntityMarshaller[Svg] =
    Marshaller.withOpenCharset(`image/svg+xml`) { (svg, charset) =>
      HttpEntity(`image/svg+xml`.withCharset(charset), svg.tag.render)
    }
}
