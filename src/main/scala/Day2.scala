import scala.io.Source
import scala.language.postfixOps

object Day2 extends GenericPuzzle("day2.csv"):
  private def toCubeCount(cube: String): (Int, Int, Int) =
    var red = 0
    var green = 0
    var blue = 0
    val splited = cube.split(", ")
    splited.foreach(color =>
      color.split(" ") match
        case tmp if tmp(1) == "red"   => red += tmp(0).toInt
        case tmp if tmp(1) == "green" => green += tmp(0).toInt
        case tmp if tmp(1) == "blue"  => blue += tmp(0).toInt
        case _                        => ()
    )
    (red, green, blue)
  private def toStructured(raw: String): (Int, Int, Int, Int) =
    val splited = raw.split(": ")
    val game = splited(0)
    val cubes = splited(1)
    val id = game.split(" ")(1).toInt
    val cubesSet = cubes.split("; ")
    val counts = cubesSet.map(toCubeCount)
    var maxRed = 0
    var maxGreen = 0
    var maxBlue = 0
    counts.foreach(count =>
      if count._1 > maxRed then maxRed = count._1
      if count._2 > maxGreen then maxGreen = count._2
      if count._3 > maxBlue then maxBlue = count._3
    )
    val result = (id, maxRed, maxGreen, maxBlue)
    result

  private val prepare = (input: List[String]) => input.map(toStructured)
  override def part1(input: List[String]): String =
    prepare(input).filter(x => x._2 <= 12 && x._3 <= 13 && x._4 <= 14).map(x => x._1).sum.toString
  override def part2(input: List[String]): String = prepare(input).map(x => x._2 * x._3 * x._4).sum.toString
