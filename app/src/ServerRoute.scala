import Extensions.QueryStringEx
import GameDirective.queryGame
import GameTypes.{Beer, Name, RoundNum}
import GameTypesUnmarshallers._
import HtmlRenderer.htmlContent
import ScalatagsMarshallers._
import TurboTags.{template, turboFrame, turboScript, turboStream}
import akka.actor.typed.scaladsl.AskPattern.{Askable, schedulerFromActorSystem}
import akka.actor.typed.{ActorRef, ActorSystem}
import akka.event.{Logging, LoggingAdapter}
import akka.http.scaladsl.marshalling.sse.EventStreamMarshalling._
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.model.sse.ServerSentEvent
import akka.http.scaladsl.server.Directives.{
  complete,
  concat,
  get,
  onSuccess,
  path,
  pathPrefix,
  post,
  redirect
}
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.server.directives.FormFieldDirectives._
import akka.stream.OverflowStrategy
import akka.stream.scaladsl.Source
import scalatags.Text.Frag
import scalatags.Text.attrs.{
  `class`,
  `type`,
  action,
  alt,
  disabled,
  href,
  id,
  method,
  name,
  placeholder,
  required,
  src,
  target,
  value
}
import scalatags.Text.implicits._
import scalatags.Text.styles.{height, width}
import scalatags.Text.tags.{
  SeqFrag,
  UnitFrag,
  a,
  b,
  form,
  frag,
  h1,
  h2,
  h3,
  img,
  input,
  label,
  li,
  p,
  ul
}
import scalatags.Text.tags2.{main, section}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.DurationInt
import scala.util.{Failure, Success}

class ServerRoute(
    joinRoute: JoinRoute,
    directory: ActorRef[Directory.Msg]
)(implicit
    system: ActorSystem[_]
) extends RouteObj {

  implicit val ec: ExecutionContext = system.executionContext
  implicit val log: LoggingAdapter =
    Logging(system.classicSystem, this)((t: ServerRoute) => "ServerRoute")

  override val route: Route = pathPrefix("server") {
    concat(
      path("stream") { // Must be first to not catch an unqualifed GET.
        get {
          handleSSE()
        }
      },
      post {
        handlePost()
      },
      get {
        handleGet()
      }
    )
  }

  private def handlePost() = queryGame(directory, system, askTimeout) {
    (code, room, oldState) =>
      formFields("event_type") {
        // Turbo requires POST submissions to result in redirects to the resulting info.
        case "assign_beer" =>
          formFields(
            "player".as[Name],
            "beer".as[Beer]
          ) { (player, beer) =>
            room ! Game.Msg.AssignBeer(player, beer)
            onSuccess(room.ask(Game.Msg.GetState)) { (state) =>
              val query = Map("code" -> code)
              redirect(s"/server?${query.queryString()}", StatusCodes.SeeOther)
            }
          }
        case "start_game" => {
          room ! Game.Msg.StartGame
          onSuccess(room.ask(Game.Msg.GetState)) { (state) =>
            val query = Map("code" -> code)
            redirect(s"/server?${query.queryString()}", StatusCodes.SeeOther)
          }
        }
        case "next_round" =>
          formFields(
            "last_round_num".as[RoundNum]
          ) { (lastRoundNum) =>
            room ! Game.Msg.BeginNextRound(lastRoundNum)
            onSuccess(room.ask(Game.Msg.GetState)) { (state) =>
              val query = Map("code" -> code)
              redirect(s"/server?${query.queryString()}", StatusCodes.SeeOther)
            }
          }
        case _ => complete(StatusCodes.BadRequest, "Unknown event_type")
      }
  }

  private def handleGet() = queryGame(directory, system, askTimeout) {
    (code, room, state) => renderPage(code, state)
  }

  private def headerScript(code: String) = {
    val query = Map("code" -> code)
    turboScript(s"/server/stream?${query.queryString()}")
  }

  private def renderPage(code: String, state: Game.State) = {
    val response = htmlContent(
      turboFrame(id := "main")(main(mainContent(code, state))),
      headerScript(code)
    )
    complete(StatusCodes.OK, response)
  }

  private def mainContent(code: String, state: Game.State): Frag = {
    state match {
      case s: Game.State.WaitingState  => waitingContent(code, s)
      case s: Game.State.WritingState  => writingContent(s)
      case s: Game.State.ReadingState  => readingContent(s)
      case s: Game.State.VotingState   => votingContent(s)
      case s: Game.State.FinRoundState => finRoundContent(s)
      case s: Game.State.FinRoomState  => finRoomContent(s)
    }
  }

  private def waitingContent(
      code: String,
      state: Game.State.WaitingState
  ) =
    frag(
      h1("Waiting Room"),
      p("Host, share this screen with everyone."),
      p(
        "Everyone, scan this code or go to ",
        a(href := joinRoute.joinUrlStr(code), target := "_blank")(
          joinRoute.publicRoot.toString
        ),
        " and enter ",
        b(code),
        " to join."
      ),
      img(
        width := "300px",
        height := "300px", {
          val query = Map("code" -> code)
          src := s"/join/qr?${query.queryString()}"
        },
        alt := "QR code linking to join page."
      ),
      p(
        "Every player is assigned a beer for their round. That round, they will lookup the true brewer's beer description and read aloud all descriptions."
      ),
      waitingStartFormContent(state),
      ul(id := "name_list")(
        for ((player, maybeBeer) <- state.playerBeers.toSeq)
          yield waitingNameContent(player, maybeBeer)
      )
    )

  private def waitingStartFormContent(state: Game.State.WaitingState) =
    form(id := "start_form", method := "POST")(
      input(`type` := "hidden", name := "event_type", value := "start_game"),
      input(
        id := "submit",
        `type` := "submit",
        value := "Start Game",
        if (!state.canStart) { disabled }
        else {}
      ),
      if (state.playerBeers.isEmpty) {
        p(`class` := "placeholder")("No players have joined... yet.")
      } else if (!state.canStart) {
        p(`class` := "placeholder")(
          "We need at least three players and all beers assigned to start."
        )
      } else {}
    )

  private def waitingNameContent(player: Name, maybeBeer: Option[Beer]) = li(
    form(method := "POST")(
      input(
        `type` := "hidden",
        name := "event_type",
        value := "assign_beer"
      ),
      input(
        `type` := "hidden",
        name := "player",
        value := player.toString
      ),
      label(
        player.toString,
        " - ",
        input(
          `type` := "text",
          name := "beer",
          placeholder := "Beer",
          required,
          value := maybeBeer.map(_.toString).getOrElse("")
        )
      ),
      input(`type` := "submit", value := "Submit")
    )
  )

  private def nameSort[V](map: Map[Name, V]): Seq[(Name, V)] =
    map.toSeq
      .sortBy { case (name, _) => name.toString }

  private def writingContent(state: Game.State.WritingState) = frag(
    h1("🍺 ", state.roundNum.toInt, ": ", state.beer.toString),
    h2("Drinking / Writing Time"),
    p("Write on your devices!"),
    ul(
      for ((player, maybeDesc) <- nameSort(state.descs))
        yield li(
          player.toString,
          " - ",
          if (maybeDesc.isDefined) { "✅" }
          else { "⏳✍️" }
        )
    )
  )

  private def readingContent(state: Game.State.ReadingState) = frag(
    h1("🍺 ", state.roundNum.toInt, ": ", state.beer.toString),
    h2("Drinking / Reading Time"),
    p(
      "Listen to ",
      state.host.toString,
      " read the descriptions."
    )
  )

  private def votingContent(state: Game.State.VotingState) = frag(
    h1("🍺 ", state.roundNum.toInt, ": ", state.beer.toString),
    h2("Drinking / Voting Time"),
    p("Vote on your devices!"),
    ul(
      for ((player, maybeDesc) <- nameSort(state.votes))
        yield li(
          player.toString,
          " - ",
          if (maybeDesc.isDefined) { "✅" }
          else { "⏳🗳" }
        )
    )
  )

  private def finRoundContent(state: Game.State.FinRoundState) = frag(
    h1(
      "🍺 ",
      state.thisRoundRecord.roundNum.toInt,
      ": ",
      state.thisRoundRecord.beer.toString
    ),
    h2("Drinking / Scoring Time"),
    h3("Round Scores"),
    RoundRecordRenderer.renderScores(state.thisRoundRecord.roundScores),
    form(method := "POST")(
      input(`type` := "hidden", name := "event_type", value := "next_round"),
      input(
        `type` := "hidden",
        name := "last_round_num",
        value := state.thisRoundRecord.roundNum.toInt
      ),
      if (state.roundsRemaining.nonEmpty) {
        input(`type` := "submit", value := "Start Next Round")
      } else {
        input(`type` := "submit", value := "Final Scores")
      }
    )
  )

  private def finRoomContent(state: Game.State.FinRoomState) = frag(
    section(
      h1("Final Scores"),
      RoundRecordRenderer.renderScores(state.finalScores)
    ),
    section(
      h1("Game Record"),
      for (roundRecord <- state.roundLog)
        yield frag(
          section(
            h2(
              "Round ",
              roundRecord.roundNum.toInt,
              ": ",
              roundRecord.beer.toString
            ),
            RoundRecordRenderer.renderRoundRecord(roundRecord)
          )
        )
    )
  )

  private def handleSSE() = queryGame(directory, system, askTimeout) {
    (code, room, state) =>
      val (queue, source) = Source
        .queue[Game.Transition](0, OverflowStrategy.dropHead)
        //.log("Server SSE Transition")
        .map(renderTransition(code))
        .map(ScalatagsMarshallers.toSSE)
        //.log("Server SSE Turbo Stream")
        .keepAlive(1.second, () => ServerSentEvent.heartbeat)
        .watchTermination() { (queue, onTerm) =>
          onTerm.onComplete {
            case Failure(ex) => {
              room ! Game.Msg.UnwatchState(queue)
              throw ex
            }
            case Success(_) => room ! Game.Msg.UnwatchState(queue)
          }
          queue
        }
        .preMaterialize()
      room ! Game.Msg.WatchState(queue)
      log.debug("New SSE stream")
      complete(source)
  }

  private def renderTransition(
      code: String
  )(transition: Game.Transition): Frag = {
    transition match {
      case Game.Transition(
            msg: Game.Msg.PlayerJoin,
            state: Game.State.WaitingState
          ) => {
        frag(
          turboStream(action := "append", target := "name_list")(
            template(waitingNameContent(msg.name, None))
          ),
          turboStream(action := "replace", target := "start_form")(
            template(waitingStartFormContent(state))
          )
        )
      }
      case Game.Transition(_, state) => {
        turboStream(action := "update", target := "main")(
          template(main(mainContent(code, state)))
        )
      }
    }
  }
}
