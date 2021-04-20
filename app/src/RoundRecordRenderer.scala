import GameTypes.{Name, RoundRecord}
import scalatags.Text.all._
import scalatags.Text.tags2.aside

object RoundRecordRenderer {

  def renderScores(scores: Map[Name, Int]): Frag = ol(
    for ((name, score) <- scoreSort(scores))
      yield li(
        name.toString,
        " - ",
        score,
        if (score != 1) { " points" }
        else { " point" }
      )
  )

  def scoreSort(scores: Map[Name, Int]): Seq[(Name, Int)] =
    scores.toSeq.sortBy { case (name, score) =>
      score
    }.reverse

  def nameSort[V](map: Map[Name, V]): Seq[(Name, V)] =
    map.toSeq
      .sortBy { case (name, _) => name.toString }

  def renderPoints(points: Map[Name, Int]): Frag = {
    val pointsToNames = points
      .groupMap { case (name, points) => points } { case (name, points) =>
        name
      }
      .filter { case (points, names) => points != 0 }
    for ((points, names) <- pointsToNames.toSeq)
      yield p(
        "+",
        points,
        if (points != 1) {
          " points"
        } else {
          " point"
        },
        " to ",
        names.map(_.toString).mkString(", "),
        "."
      )
  }

  def renderRoundRecord(roundRecord: RoundRecord): Frag = frag(
    p("The host this round was ", roundRecord.realEntry.writer.toString, "."),
    h3(
      "Real: Description ",
      roundRecord.realEntry.index.toInt
    ),
    blockquote(roundRecord.realEntry.desc.toString),
    aside(
      p(if (roundRecord.realEntry.voters.nonEmpty) {
        frag(
          if (roundRecord.realEntry.voters.size != 1) {
            "Votes"
          } else {
            "A vote"
          },
          " from ",
          roundRecord.realEntry.voters.map(_.toString).mkString(", "),
          "."
        )
      } else {
        "No votes."
      }),
      if (roundRecord.realEntry.points.nonEmpty) {
        renderPoints(roundRecord.realEntry.points)
      } else {}
    ),
    for (fakeEntry <- roundRecord.fakeEntries.sortBy(_.voters.size).reverse)
      yield frag(
        h3(
          "Fake: Description ",
          fakeEntry.index.toInt,
          " by ",
          fakeEntry.writer.toString
        ),
        blockquote(fakeEntry.desc.toString),
        aside(
          p(if (fakeEntry.voters.nonEmpty) {
            frag(
              if (fakeEntry.voters.size != 1) {
                "Votes"
              } else {
                "A vote"
              },
              " from ",
              fakeEntry.voters.map(_.toString).mkString(", "),
              "."
            )
          } else {
            "No votes."
          }),
          if (fakeEntry.points.nonEmpty) {
            renderPoints(fakeEntry.points)
          } else {}
        )
      )
  )
}
