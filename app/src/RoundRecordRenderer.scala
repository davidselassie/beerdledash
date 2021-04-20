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

  def renderRoundRecord(roundRecord: RoundRecord): Frag = frag(
    p("The host this round was ", roundRecord.trueEntry.writer.toString, "."),
    h3(
      "Actual: Description ",
      roundRecord.trueEntry.index.toInt
    ),
    blockquote(roundRecord.trueEntry.desc.toString),
    aside(
      if (roundRecord.trueEntry.voters.nonEmpty) {
        frag(
          p(
            if (roundRecord.trueEntry.voters.size != 1) {
              "Votes"
            } else {
              "A vote"
            },
            " from ",
            roundRecord.trueEntry.voters.map(_.toString).mkString(", "),
            "."
          ),
          p(
            "+2 points to ",
            roundRecord.trueEntry.voters.map(_.toString).mkString(", "),
            "."
          )
        )
      } else {
        frag(
          p("No votes."),
          p(
            "+3 points to ",
            roundRecord.trueEntry.writer.toString,
            "."
          )
        )
      }
    ),
    for (entry <- roundRecord.fakeEntries.sortBy(_.voters.size).reverse)
      yield frag(
        h3(
          "Fake: Description ",
          entry.index.toInt,
          " by ",
          entry.writer.toString
        ),
        blockquote(entry.desc.toString),
        aside(
          if (entry.voters.nonEmpty) {
            frag(
              p(
                if (roundRecord.trueEntry.voters.size != 1) {
                  "Votes"
                } else {
                  "A vote"
                },
                " from ",
                entry.voters.map(_.toString).mkString(", "),
                "."
              ),
              p(
                "+",
                entry.voters.size,
                if (entry.voters.size != 1) {
                  " points"
                } else {
                  " point"
                },
                " to ",
                entry.writer.toString,
                "."
              )
            )
          } else {
            p("No votes.")
          }
        )
      )
  )
}
