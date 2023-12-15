object Day11 extends GenericPuzzle("day11.csv"):
  private def toMap(raw: List[String]) = raw.map(_.toCharArray.toList)
  private def findGalaxies(map: List[List[Char]]) = (for
    x <- map.indices
    y <- map.head.indices
  yield (x, y, map(x)(y))).filter(_._3 == '#').map(x => (x._1, x._2)).toList
  private def findEmpty(map: List[List[Char]]) =
    val emptyRow = map.zipWithIndex.filter(_._1.forall(_ == '.')).map(_._2)
    val emptyColumn = map.head.indices.filter(y => map.forall(_(y) == '.')).toList
    (emptyRow, emptyColumn)
  private def generatePairs(galaxies: List[(Int, Int)]) = (for
    i <- galaxies.indices
    j <- galaxies.indices.filter(_ > i)
  yield (galaxies(i), galaxies(j))).toList
  private def calculateDistance(pair: ((Int, Int), (Int, Int)), empties: (List[Int], List[Int]), times: Int) =
    BigInt(Math.abs(pair._1._1 - pair._2._1))
      + BigInt(Math.abs(pair._1._2 - pair._2._2))
      + BigInt(
        empties._1.count(
          Range(pair._1._1, pair._2._1, if pair._1._1 > pair._2._1 then -1 else 1).contains
        ) * (times - 1)
      )
      + BigInt(
        empties._2.count(
          Range(pair._1._2, pair._2._2, if pair._1._2 > pair._2._2 then -1 else 1).contains
        ) * (times - 1)
      )

  override def part1(input: List[String]): String =
    val map = toMap(input)
    generatePairs(findGalaxies(map))
      .map(calculateDistance(_, findEmpty(map), 2))
      .sum
      .toString
  override def part2(input: List[String]): String =
    val map = toMap(input)
    generatePairs(findGalaxies(map))
      .map(calculateDistance(_, findEmpty(map), 1000000))
      .sum
      .toString
