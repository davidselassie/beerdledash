import akka.http.scaladsl.marshalling.{Marshaller, ToEntityMarshaller}
import akka.http.scaladsl.model.sse.ServerSentEvent
import akka.http.scaladsl.model.{ContentTypes, HttpEntity}
import scalatags.Text
import scalatags.Text.all.Frag

object ScalatagsMarshallers {

  implicit val FragMarshaller: ToEntityMarshaller[Frag] =
    Marshaller.withFixedContentType(ContentTypes.`text/html(UTF-8)`) { (frag) =>
      HttpEntity(ContentTypes.`text/html(UTF-8)`, frag.render)
    }

  implicit val DoctypeMarshaller: ToEntityMarshaller[Text.all.doctype] =
    Marshaller.withFixedContentType(ContentTypes.`text/html(UTF-8)`) { (dt) =>
      HttpEntity(ContentTypes.`text/html(UTF-8)`, dt.render)
    }

  def toSSE(frag: Frag): ServerSentEvent = ServerSentEvent(frag.render)

  def toSSE(doctype: Text.all.doctype): ServerSentEvent = ServerSentEvent(
    doctype.render
  )
}
