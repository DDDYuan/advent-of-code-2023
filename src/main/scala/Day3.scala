import scala.io.Source
import scala.language.postfixOps

object Day3 extends GenericPuzzle("day3.csv"):
  private val regex = """[^0-9]*(\d+)[^0-9]*""".r
  private val symbol = """[^0-9.]""".r
  private def toExtracted(input: (String, Int)): List[(Int, Int, Int, Int)] =
    regex.findAllMatchIn(input._1).map(m => (m.group(1).toInt, input._2, m.start(1), m.end(1) - 1)).toList
  private def isPositionValid(position: (Int, Int), maxRow: Int, maxColumn: Int): Boolean =
    position._1 <= maxRow && position._2 <= maxColumn && position._1 >= 0 && position._2 >= 0
  private def partNumberCheckpoints(row: Int, start: Int, to: Int): List[(Int, Int)] =
    List((row, start - 1), (row, to + 1)) ::: (start - 1 to to + 1)
      .flatMap(column => List((row - 1, column), (row + 1, column)))
      .toList
  private def isPartNumber(number: (Int, Int, Int, Int), map: List[String]): Boolean =
    partNumberCheckpoints(number._2, number._3, number._4)
      .filter(isPositionValid(_, map.size - 1, map.head.length - 1))
      .exists(position => symbol.matches(map(position._1).charAt(position._2).toString))
  private def possibleGears(number: (Int, Int, Int, Int), map: List[String]): List[(Int, Int)] =
    partNumberCheckpoints(number._2, number._3, number._4)
      .filter(isPositionValid(_, map.size - 1, map.head.length - 1))
      .filter(position => map(position._1).charAt(position._2).toString == "*")

  private val prepare = (input: List[String]) => input.zipWithIndex.flatMap(toExtracted)
  override def part1(input: List[String]): String = prepare(input)
    .filter(isPartNumber(_, input))
    .map(_._1)
    .sum
    .toString
  override def part2(input: List[String]): String = prepare(input)
    .flatMap(n => possibleGears(n, input).map((n._1, _)))
    .groupBy(_._2)
    .filter(entry => entry._2.size == 2)
    .view
    .mapValues(n => n.map(_._1).product)
    .values
    .sum
    .toString
