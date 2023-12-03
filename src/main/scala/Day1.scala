import scala.io.Source

object Day1 extends GenericPuzzle("day1.csv"):
  private val letters = List(
    "one",
    "two",
    "three",
    "four",
    "five",
    "six",
    "seven",
    "eight",
    "nine",
    "1",
    "2",
    "3",
    "4",
    "5",
    "6",
    "7",
    "8",
    "9"
  )
  private val numbers = List("1", "2", "3", "4", "5", "6", "7", "8", "9")
  private def firstMatch(template: List[String])(raw: String): Int =
    val result = template
      .map(raw.indexOf)
      .map(index => if index < 0 then Int.MaxValue else index)
      .zipWithIndex
      .minBy(entry => entry._1)
      ._2 + 1
    if result > 9 then result - 9 else result
  private def lastMatch(template: List[String])(raw: String): Int =
    val result = template
      .map(raw.lastIndexOf)
      .zipWithIndex
      .maxBy(entry => entry._1)
      ._2 + 1
    if result > 9 then result - 9 else result
  private def toNumber(template: List[String])(raw: String): Int =
    firstMatch(template)(raw) * 10 + lastMatch(template)(raw)
  override def part1(input: List[String]): String = input.map(toNumber(numbers)).sum.toString
  override def part2(input: List[String]): String = input.map(toNumber(letters)).sum.toString
