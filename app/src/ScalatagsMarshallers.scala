import akka.http.scaladsl.marshalling.{Marshaller, ToEntityMarshaller}
import akka.http.scaladsl.model.sse.ServerSentEvent
import akka.http.scaladsl.model.{HttpEntity, MediaTypes}
import scalatags.Text.Frag
import scalatags.Text.tags.doctype

object ScalatagsMarshallers {

  implicit val FragMarshaller: ToEntityMarshaller[Frag] =
    Marshaller.withOpenCharset(MediaTypes.`text/html`) { (frag, charset) =>
      HttpEntity(MediaTypes.`text/html`.withCharset(charset), frag.render)
    }

  implicit val DoctypeMarshaller: ToEntityMarshaller[doctype] =
    Marshaller.withOpenCharset(MediaTypes.`text/html`) { (dt, charset) =>
      HttpEntity(MediaTypes.`text/html`.withCharset(charset), dt.render)
    }

  def toSSE(frag: Frag): ServerSentEvent = ServerSentEvent(frag.render)

  def toSSE(doctype: doctype): ServerSentEvent = ServerSentEvent(
    doctype.render
  )
}
