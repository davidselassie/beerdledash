import Extensions.QueryStringEx
import GameDirective.{queryGame, queryPlayer}
import GameTypes._
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
  formFields,
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
  checked,
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
import scalatags.Text.tags.{
  SeqFrag,
  UnitFrag,
  a,
  b,
  blockquote,
  div,
  form,
  frag,
  h1,
  h2,
  h3,
  h4,
  img,
  input,
  label,
  li,
  p,
  span,
  textarea,
  ul,
  code => codetag
}
import scalatags.Text.tags2.{aside, main, section}

import java.net.URLEncoder
import java.nio.charset.StandardCharsets
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.DurationInt
import scala.util.{Failure, Success}

class GameRoute(joinRoute: JoinRoute, directory: ActorRef[Directory.Msg])(
    implicit system: ActorSystem[_]
) extends RouteObj {

  implicit val ec: ExecutionContext = system.executionContext
  implicit val log: LoggingAdapter =
    Logging(system.classicSystem, this)((t: GameRoute) => "GameRoute")

  override val route: Route = pathPrefix("game") {
    concat(
      path("stream") { // Must be first to not catch an unqualifed GET.
        handleSSE()
      },
      post {
        handlePost()
      },
      get {
        handleGet()
      }
    )
  }

  private def handlePost() = queryPlayer { (player) =>
    queryGame(directory, system, askTimeout) { (code, room, oldState) =>
      val query = Map(
        "code" -> code,
        "name" -> player.toString
      )
      formFields("event_type") {
        // Turbo requires POST submissions to result in redirects to the resulting info.
        case "assign_beer" =>
          formFields(
            "player".as[Name],
            "beer".as[Beer]
          ) { (player, beer) =>
            room ! Game.Msg.AssignBeer(player, beer)
            onSuccess(room.ask(Game.Msg.GetState)) { (state) =>
              redirect(s"/game?${query.queryString()}", StatusCodes.SeeOther)
            }
          }
        case "start_game" => {
          room ! Game.Msg.StartGame
          onSuccess(room.ask(Game.Msg.GetState)) { (state) =>
            redirect(s"/game?${query.queryString()}", StatusCodes.SeeOther)
          }
        }
        case "next_round" =>
          formFields(
            "last_round_num".as[RoundNum]
          ) { (lastRoundNum) =>
            room ! Game.Msg.BeginNextRound(lastRoundNum)
            onSuccess(room.ask(Game.Msg.GetState)) { (state) =>
              redirect(s"/game?${query.queryString()}", StatusCodes.SeeOther)
            }
          }
        case "submit_desc" =>
          formFields("round_num".as[RoundNum], "desc".as[Desc]) {
            (roundNum, desc) =>
              room ! Game.Msg.SubmitDesc(roundNum, player, desc)
              onSuccess(room.ask(Game.Msg.GetState)) { (state) =>
                redirect(
                  s"/game?${query.queryString()}",
                  StatusCodes.SeeOther
                )
              }
          }
        case "read_next" => {
          formFields(
            "round_num".as[RoundNum],
            "last_index".as[BallotIndex]
          ) { (roundNum, lastIndex) =>
            room ! Game.Msg.ReadNext(roundNum, lastIndex)
            onSuccess(room.ask(Game.Msg.GetState)) { (state) =>
              redirect(s"/game?${query.queryString()}", StatusCodes.SeeOther)
            }
          }
        }
        case "record_vote" =>
          formFields(
            "round_num".as[RoundNum],
            "vote_index".as[BallotIndex]
          ) { (roundNum, index) =>
            room ! Game.Msg.RecordVote(roundNum, player, index)
            onSuccess(room.ask(Game.Msg.GetState)) { (state) =>
              redirect(s"/game?${query.queryString()}", StatusCodes.SeeOther)
            }
          }
        case _ => complete(StatusCodes.BadRequest, "Unknown event_type")
      }
    }
  }

  private def handleGet() = queryPlayer { (player) =>
    queryGame(directory, system, askTimeout) { (code, room, state) =>
      renderPage(code, player, state)
    }
  }

  private def headerScript(code: String, player: Name) = {
    val query = Map("code" -> code, "name" -> player.toString)
    turboScript(s"/game/stream?${query.queryString()}")
  }

  private def renderPage(code: String, player: Name, state: Game.State) = {
    val response = htmlContent(
      turboFrame(id := "main")(main(mainContent(code, player, state))),
      headerScript(code, player)
    )
    complete(StatusCodes.OK, response)
  }

  private def mainContent(
      code: String,
      player: Name,
      state: Game.State
  ): Frag = {
    state match {
      case s: Game.State.WaitingState => waitingContent(code, player, s)
      case s: Game.State.WritingState if s.host == player =>
        hostWritingContent(player, s)
      case s: Game.State.WritingState => guestWritingContent(player, s)
      case s: Game.State.ReadingState if s.host == player =>
        hostReadingContent(s)
      case s: Game.State.ReadingState                    => guestReadingContent(s)
      case s: Game.State.VotingState if s.host == player => hostVotingContent(s)
      case s: Game.State.VotingState                     => guestVotingContent(player, s)
      case s: Game.State.FinRoundState                   => finRoundContent(player, s)
      case s: Game.State.FinRoomState                    => finRoomContent(s)
    }
  }

  private def waitingContent(
      code: String,
      player: Name,
      state: Game.State.WaitingState
  ) =
    frag(
      h1("Waiting Room ", codetag(code)),
      p(
        "New players can scan this barcode or ",
        a(href := joinRoute.joinUrlStr(code), target := "_blank")(
          "go to ",
          codetag(joinRoute.publicRoot.toString),
          " and enter ",
          codetag(code),
          " to join"
        ),
        "."
      ),
      div(`class` := "center")(
        img(
          `class` := "qr", {
            val query = Map("code" -> code)
            src := s"/join/qr?${query.queryString()}"
          },
          alt := "QR code to join game."
        )
      ),
      p(
        "Every player is assigned a beer for their round. That round, they will lookup the true brewer's beer description and read aloud all descriptions."
      ),
      h2("Players and Beers"),
      form(id := "assign", method := "POST")(
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
          "Which beer will we drink your round? ",
          input(
            `type` := "text",
            name := "beer",
            placeholder := "PBR",
            required,
            state.playerBeers.getOrElse(player, None) match {
              case Some(beer) => value := beer.toString
              case None       => {}
            }
          )
        ),
        p(
          input(`type` := "submit", value := "Assign"),
          if (state.playerBeers.getOrElse(player, None).isDefined) {
            " ✅"
          } else {}
        )
      ),
      ul(id := "names")(
        for ((player, maybeBeer) <- state.playerBeers.toSeq)
          yield waitingNameContent(player, maybeBeer)
      ),
      waitingStartFormContent(player, state)
    )

  private def waitingStartFormContent(
      player: Name,
      state: Game.State.WaitingState
  ) = div(id := "start_form")(
    h2("Start Game"),
    if (state.leader.contains(player)) {
      form(method := "POST")(
        input(`type` := "hidden", name := "event_type", value := "start_game"),
        input(
          id := "submit",
          `type` := "submit",
          value := "Start Game",
          if (!state.canStart) {
            disabled
          } else {}
        )
      )
    },
    aside(
      p(
        if (!state.canStart) {
          "We need at least three players and everyone assigned a beer to start."
        } else {
          state.leader match {
            case None =>
              "No players have joined... yet."
            case Some(leader) if leader == player => {}
            case Some(leader)                     => frag(leader.toString, " can start the game.")
          }
        }
      )
    )
  )

  private def waitingNameContent(player: Name, maybeBeer: Option[Beer]) =
    li(id := s"${player.toString}_beer")(
      player.toString,
      ": ",
      maybeBeer match {
        case Some(beer) => beer.toString
        case None       => span(`class` := "aside")("Needs a beer.")
      }
    )

  private def hostWritingContent(player: Name, state: Game.State.WritingState) =
    frag(
      h1("🍺 ", state.roundNum.toInt, ": ", state.beer.toString),
      h2("Drinking / Writing Time"),
      p(
        "This round you're the host! Look up the actual brewer's description of ",
        b(state.beer.toString),
        " and submit it below. ", {
          val query = s"${state.beer.toString} beer description"
          a(
            href := s"https://www.google.com/search?q=${URLEncoder
              .encode(query, StandardCharsets.UTF_8.name())}",
            target := "_blank"
          )("I can Google it for you.")
        }
      ),
      p(
        "You can re-submit the description until everyone has submitted theirs."
      ),
      form(method := "POST")(
        input(`type` := "hidden", name := "event_type", value := "submit_desc"),
        input(
          `type` := "hidden",
          name := "round_num",
          value := state.roundNum.toInt
        ),
        textarea(
          name := "desc",
          placeholder := "Actual brewer's beer description...",
          required
        )(
          state.descs.getOrElse(player, None).getOrElse(Desc("")).toString
        ),
        p(
          input(`type` := "submit", value := "Submit"),
          if (state.descs.getOrElse(player, None).isDefined) {
            " ✅"
          } else {}
        )
      )
    )

  private def guestWritingContent(
      player: Name,
      state: Game.State.WritingState
  ) =
    frag(
      h1("🍺 ", state.roundNum.toInt, ": ", state.beer.toString),
      h2("Drinking / Writing Time"),
      p(
        "Time to be crafty. Drink some ",
        b(state.beer.toString),
        " and write a beer description that will trick others into thinking it came from the actual brewer."
      ),
      p(
        "You can re-submit your description until everyone has submitted theirs."
      ),
      form(method := "POST")(
        input(`type` := "hidden", name := "event_type", value := "submit_desc"),
        input(
          `type` := "hidden",
          name := "round_num",
          value := state.roundNum.toInt
        ),
        textarea(
          name := "desc",
          placeholder := "Your fake beer description...",
          required
        )(
          state.descs.getOrElse(player, None).getOrElse(Desc("")).toString
        ),
        p(
          input(`type` := "submit", value := "Submit"),
          if (state.descs.getOrElse(player, None).isDefined) {
            " ✅"
          } else {}
        )
      )
    )

  private def hostReadingContent(state: Game.State.ReadingState) = frag(
    h1("🍺 ", state.roundNum.toInt, ": ", state.beer.toString),
    h2("Drinking / Reading Time"),
    p(
      "Since you're the host this round, read each description aloud to everyone."
    ),
    h3("Description ", state.entry.index.toInt),
    blockquote(state.entry.desc.toString),
    form(method := "POST")(
      input(`type` := "hidden", name := "event_type", value := "read_next"),
      input(
        `type` := "hidden",
        name := "round_num",
        value := state.roundNum.toInt
      ),
      input(
        `type` := "hidden",
        name := "last_index",
        value := state.entry.index.toInt
      ),
      state.entriesRemaining match {
        case _ :: _ => input(`type` := "submit", value := "Read Next")
        case Seq()  => input(`type` := "submit", value := "Begin Voting")
      }
    )
  )

  private def guestReadingContent(state: Game.State.ReadingState) = frag(
    h1("🍺 ", state.roundNum.toInt, ": ", state.beer.toString),
    h2("Drinking / Reading Time"),
    p(
      "Listen to ",
      state.host.toString,
      " read the descriptions and think about which is from the real brewer."
    ),
    p("Don't worry, you'll get to see them all at the end."),
    h4("Description ", state.entry.index.toInt),
    blockquote(state.entry.desc.toString)
  )

  private def hostVotingContent(state: Game.State.VotingState) = frag(
    h1("🍺 ", state.roundNum.toInt, ": ", state.beer.toString),
    h2("Drinking / Voting Time"),
    p(
      "Since you're the host this round, you don't vote. Wait until everyone else is done."
    ),
    p(
      "You'll score points if no one correctly identifies the actual description."
    ),
    for (entry <- state.ballot)
      yield frag(
        h3("Description ", entry.index.toInt),
        blockquote(entry.desc.toString)
      )
  )

  private def guestVotingContent(player: Name, state: Game.State.VotingState) =
    frag(
      h1("🍺 ", state.roundNum.toInt, ": ", state.beer.toString),
      h2("Drinking / Voting Time"),
      p(
        "You'll score points by voting for the actual brewer's description."
      ),
      p("If you pick a fake description, the writer will get points instead!"),
      p("You can re-cast your ballot until everyone has submitted theirs."),
      form(method := "POST")(
        input(`type` := "hidden", name := "event_type", value := "record_vote"),
        input(
          `type` := "hidden",
          name := "round_num",
          value := state.roundNum.toInt
        ),
        for (entry <- state.ballot)
          yield {
            val isChecked = state.votes
              .getOrElse(player, None)
              .contains(entry.index)
            frag(
              h3("Description ", entry.index.toInt),
              blockquote(entry.desc.toString),
              label(`class` := "placeholder")(
                input(
                  `type` := "radio",
                  name := "vote_index",
                  value := entry.index.toInt,
                  required,
                  if (isChecked) {
                    checked
                  } else {}
                ),
                " Vote for description ",
                entry.index.toInt
              )
            )
          },
        p(
          input(`type` := "submit", value := "Cast Ballot"),
          if (
            state.votes
              .getOrElse(player, None)
              .isDefined
          ) {
            " ✅"
          } else {}
        )
      )
    )

  private def finRoundContent(player: Name, state: Game.State.FinRoundState) =
    frag(
      h1(
        "🍺 ",
        state.thisRoundRecord.roundNum.toInt,
        ": ",
        state.thisRoundRecord.beer.toString
      ),
      h2("Drinking / Scoring Time"),
      RoundRecordRenderer.renderRoundRecord(state.thisRoundRecord),
      h2("Next Round"),
      state.roundsRemaining match {
        case nextRound :: _ if nextRound.host == player =>
          frag(
            p("You are hosting the next round."),
            form(method := "POST")(
              input(
                `type` := "hidden",
                name := "event_type",
                value := "next_round"
              ),
              input(
                `type` := "hidden",
                name := "last_round_num",
                value := state.thisRoundRecord.roundNum.toInt
              ),
              input(`type` := "submit", value := "Start Round")
            )
          )
        case nextRound :: _ =>
          frag(
            p(nextRound.host.toString, " is hosting the next round."),
            aside(p("They can start it on their device."))
          )
        case Seq() =>
          frag(
            p("That was the last round."),
            if (state.leader == player) {
              form(method := "POST")(
                input(
                  `type` := "hidden",
                  name := "event_type",
                  value := "next_round"
                ),
                input(
                  `type` := "hidden",
                  name := "last_round_num",
                  value := state.thisRoundRecord.roundNum.toInt
                ),
                input(`type` := "submit", value := "Final Scores")
              )
            } else {
              aside(
                p(
                  state.leader.toString,
                  " can take us to the final scores."
                )
              )
            }
          )
      }
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
              "🍺 ",
              roundRecord.roundNum.toInt,
              ": ",
              roundRecord.beer.toString
            ),
            RoundRecordRenderer.renderRoundRecord(roundRecord)
          )
        )
    )
  )

  private def handleSSE() = queryPlayer { (player) =>
    queryGame(directory, system, askTimeout) { (code, room, state) =>
      val (queue, source) = Source
        .queue[Game.Transition](0, OverflowStrategy.dropHead)
        //.log("SSE Transition")
        .map(renderTransition(code, player))
        .map(ScalatagsMarshallers.toSSE)
        //.log("SSE Turbo Stream")
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
  }

  private def renderTransition(
      code: String,
      player: Name
  )(transition: Game.Transition): Frag = {
    transition match {
      case Game.Transition(
            msg: Game.Msg.PlayerJoin,
            state: Game.State.WaitingState
          ) => {
        frag(
          turboStream(action := "append", target := "names")(
            template(waitingNameContent(msg.name, None))
          ),
          turboStream(action := "replace", target := "start_form")(
            template(waitingStartFormContent(player, state))
          )
        )
      }
      case Game.Transition(
            msg: Game.Msg.AssignBeer,
            state: Game.State.WaitingState
          ) => {
        frag(
          turboStream(
            action := "replace",
            target := s"${msg.name.toString}_beer"
          )(
            template(waitingNameContent(msg.name, Some(msg.beer)))
          ),
          turboStream(action := "replace", target := "start_form")(
            template(waitingStartFormContent(player, state))
          )
        )
      }
      // Ignore these since no info is displayed about other players on a given player's device.
      // We don't want to overwrite un-submitted input with a refresh.
      case Game.Transition(
            _: Game.Msg.SubmitDesc,
            _: Game.State.WritingState
          ) => {}
      case Game.Transition(
            _: Game.Msg.RecordVote,
            _: Game.State.VotingState
          ) => {}
      case Game.Transition(_, state) => {
        turboStream(action := "update", target := "main")(
          template(main(mainContent(code, player, state)))
        )
      }
    }
  }
}
