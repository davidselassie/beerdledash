import akka.http.scaladsl.marshalling.{Marshaller, ToEntityMarshaller}
import akka.http.scaladsl.model.sse.ServerSentEvent
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, MediaTypes}
import akka.http.scaladsl.server.ContentNegotiator.Alternative.MediaType
import scalatags.Text
import scalatags.Text.all.Frag

object ScalatagsMarshallers {

  implicit val FragMarshaller: ToEntityMarshaller[Frag] =
    Marshaller.withOpenCharset(MediaTypes.`text/html`) { (frag, charset) =>
      HttpEntity(MediaTypes.`text/html`.withCharset(charset), frag.render)
    }

  implicit val DoctypeMarshaller: ToEntityMarshaller[Text.all.doctype] =
    Marshaller.withOpenCharset(MediaTypes.`text/html`) { (dt, charset) =>
      HttpEntity(MediaTypes.`text/html`.withCharset(charset), dt.render)
    }

  def toSSE(frag: Frag): ServerSentEvent = ServerSentEvent(frag.render)

  def toSSE(doctype: Text.all.doctype): ServerSentEvent = ServerSentEvent(
    doctype.render
  )
}
