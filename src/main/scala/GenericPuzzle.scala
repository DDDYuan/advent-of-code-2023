import scala.io.Source

trait GenericPuzzle(file: String):
  private def load: List[String] = Source.fromResource(file).getLines().toList
  def part1(input: List[String]): String = "Not implemented"
  def part2(input: List[String]): String = "Not implemented"

  def solve(): Unit =
    val input = load
    println(s"Starting calculating for input: $file...")
    val result1 = part1(input)
    println(s"Part1 result: $result1")
    val result2 = part2(input)
    println(s"Part2 result: $result2")
