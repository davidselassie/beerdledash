object GameTypes {

  final case class Code(override val toString: String) {
    require(toString.length == Code.Length, "code not 4 characters")
  }
  object Code {
    val Length = 4
  }

  final case class RoundNum(toInt: Int)

  final case class Name(override val toString: String) {
    require(toString.length <= Name.MaxLength, "name too long")
  }
  object Name {
    val MaxLength = 64
  }

  final case class Beer(override val toString: String) {
    require(toString.length <= Beer.MaxLength, "beer name too long")
  }
  object Beer {
    val MaxLength = 128
  }

  final case class Desc(override val toString: String) {
    require(toString.length <= Desc.MaxLength, "description too long")
  }
  object Desc {
    val MaxLength = 2048
  }

  final case class BallotIndex(toInt: Int)

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
