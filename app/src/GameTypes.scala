object GameTypes {

  final case class RoundNum(toInt: Int) extends AnyVal
  final case class Name(override val toString: String) extends AnyVal
  final case class Beer(override val toString: String) extends AnyVal
  final case class Desc(override val toString: String) extends AnyVal
  final case class BallotIndex(toInt: Int) extends AnyVal

  final case class Round(num: RoundNum, host: Name, beer: Beer)
  final case class RoundRecord(
      roundNum: RoundNum,
      beer: Beer,
      realEntry: ScoredBallotEntry,
      fakeEntries: Seq[ScoredBallotEntry],
      roundScores: Map[Name, Int]
  )
  final case class BallotEntry(index: BallotIndex, desc: Desc, writer: Name)
  final case class ScoredBallotEntry(
      index: BallotIndex,
      desc: Desc,
      writer: Name,
      voters: Set[Name],
      points: Map[Name, Int]
  )
}
