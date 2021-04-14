import akka.http.scaladsl.marshalling.{Marshaller, ToEntityMarshaller}
import akka.http.scaladsl.model.{HttpEntity, MediaType}
import com.google.zxing.qrcode.encoder.QRCode
import org.jfree.graphics2d.svg.SVGGraphics2D

import java.awt.Color

object QRCodeToSVGWriter {

  def render(qr: QRCode, scale: Int = 1): SVGGraphics2D = {
    val mat = qr.getMatrix

    val ctx = new SVGGraphics2D(mat.getWidth * scale, mat.getHeight * scale)
    ctx.setColor(Color.BLACK)

    for (
      y <- Range(0, mat.getHeight);
      x <- Range(0, mat.getWidth)
    ) {
      if (mat.get(x, y) > 0) {
        ctx.fillRect(x * scale, y * scale, scale, scale)
      }
    }

    ctx
  }

  val `image/svg+xml`: MediaType.WithOpenCharset =
    MediaType.customWithOpenCharset("image", "svg+xml", List("svg"))

  implicit val SVGGraphics2DMarshaller: ToEntityMarshaller[SVGGraphics2D] =
    Marshaller.withOpenCharset(`image/svg+xml`) { (ctx, charset) =>
      HttpEntity(`image/svg+xml`.withCharset(charset), ctx.getSVGDocument)
    }
}
