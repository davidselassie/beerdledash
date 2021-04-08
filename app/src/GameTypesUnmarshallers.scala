import GameTypes.{BallotIndex, Beer, Desc, Name, RoundNum}
import akka.http.scaladsl.unmarshalling.{
  PredefinedFromStringUnmarshallers,
  Unmarshaller
}

object GameTypesUnmarshallers {

  implicit val RoundNumUnmarshaller: Unmarshaller[String, RoundNum] =
    PredefinedFromStringUnmarshallers.intFromStringUnmarshaller
      .map((int) => RoundNum(int))

  implicit val BallotIndexUnmarshaller: Unmarshaller[String, BallotIndex] =
    PredefinedFromStringUnmarshallers.intFromStringUnmarshaller
      .map((int) => BallotIndex(int))

  implicit val NameUnmarshaller: Unmarshaller[String, Name] =
    Unmarshaller.strict((str) => Name(str))

  implicit val BeerUnmarshaller: Unmarshaller[String, Beer] =
    Unmarshaller.strict((str) => Beer(str))

  implicit val DescUnmarshaller: Unmarshaller[String, Desc] =
    Unmarshaller.strict((str) => Desc(str))
}
