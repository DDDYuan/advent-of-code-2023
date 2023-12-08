import scala.annotation.tailrec

object Day8 extends GenericPuzzle("day8.csv"):
  private val regex = """^([0-9A-Z]{3}) = \(([0-9A-Z]{3}), ([0-9A-Z]{3})\)$""".r
  private def toStructured(raw: String): (String, (String, String)) =
    val ma = regex.findFirstMatchIn(raw).get
    (ma.group(1), (ma.group(2), ma.group(3)))
  private def findSteps(
    start: String,
    end: String => Boolean,
    maps: Map[String, (String, String)],
    ins: Array[Char]
  ): BigInt =
    var current = start
    var steps = 0
    while !end(current) do
      val next = maps(current)
      ins(steps % ins.length) match
        case 'L' => current = next._1
        case 'R' => current = next._2
      steps = steps + 1
    steps
  @tailrec
  private def gcd(x: BigInt, y: BigInt): BigInt = if y == 0 then x else gcd(y, x % y)
  private def lcm(x: BigInt, y: BigInt) = x * y / gcd(x, y)
  private def lcms(numbers: List[BigInt]) = numbers.foldLeft(BigInt(1))(lcm)
  override def part1(input: List[String]): String =
    val ins = input.head.toCharArray
    val maps = input.drop(2).map(toStructured).toMap
    findSteps("AAA", _ == "ZZZ", maps, ins).toString
  override def part2(input: List[String]): String =
    val ins = input.head.toCharArray
    val maps = input.drop(2).map(toStructured).toMap
    lcms(maps.keys.filter(_.endsWith("A")).map(findSteps(_, _.endsWith("Z"), maps, ins)).toList).toString
