import scala.annotation.tailrec

object Day6 extends GenericPuzzle("day6.csv"):
  private def toNumber(raw: String) = raw.split("\\s+").drop(1).map(BigInt(_))
  private def toPair(raw: List[String]) =
    val time = toNumber(raw.head)
    val distance = toNumber(raw.last)
    time zip distance
  private def toSingleNumber(raw: String) = BigInt(raw.split(":").last.replaceAll(" ", ""))
  private def toSinglePair(raw: List[String]) = (toSingleNumber(raw.head), toSingleNumber(raw.last))
  @tailrec
  private def findMatch(start: BigInt, pair: (BigInt, BigInt)): BigInt =
    if start * (pair._1 - start) > pair._2 then start else findMatch(start + 1, pair)

  private def possibleWays(pair: (BigInt, BigInt)) =
    (pair._1 / 2 - findMatch(1, pair) + 1) * 2 - (if pair._1 % 2 == 0 then 1 else 0)
  override def part1(input: List[String]): String = toPair(input).map(possibleWays).product.toString
  override def part2(input: List[String]): String = possibleWays(toSinglePair(input)).toString
