import GameTypes.{Name, RoundRecord}
import scalatags.Text.all._

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

  def renderRoundRecord(roundRecord: RoundRecord): Frag = frag(
    h3(
      "Actual: Description ",
      roundRecord.trueEntry.index.toInt,
      " by ",
      roundRecord.trueEntry.writer.toString
    ),
    blockquote(roundRecord.trueEntry.desc.toString),
    if (roundRecord.trueEntry.voters.nonEmpty) {
      frag(
        p(`class` := "placeholder")(
          if (roundRecord.trueEntry.voters.size != 1) {
            "Votes"
          } else {
            "A vote"
          },
          " from ",
          roundRecord.trueEntry.voters.map(_.toString).mkString(", "),
          "."
        ),
        p(`class` := "placeholder")(
          "+2 points to ",
          roundRecord.trueEntry.voters.map(_.toString).mkString(", "),
          "."
        )
      )
    } else {
      frag(
        p(`class` := "placeholder")("No votes."),
        p(`class` := "placeholder")(
          "+3 points to ",
          roundRecord.trueEntry.writer.toString,
          "."
        )
      )
    },
    for (entry <- roundRecord.fakeEntries.sortBy(_.voters.size).reverse)
      yield frag(
        h3(
          "Fake: Description ",
          entry.index.toInt,
          " by ",
          entry.writer.toString
        ),
        blockquote(entry.desc.toString),
        if (entry.voters.nonEmpty) {
          frag(
            p(`class` := "placeholder")(
              if (roundRecord.trueEntry.voters.size != 1) {
                "Votes"
              } else {
                "A vote"
              },
              " from ",
              entry.voters.map(_.toString).mkString(", "),
              "."
            ),
            p(`class` := "placeholder")(
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
          p(`class` := "placeholder")("No votes.")
        }
      )
  )
}
