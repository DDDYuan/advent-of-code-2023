object Day4 extends GenericPuzzle("day4.csv"):
  private def toSeparated(raw: String): (List[Int], List[Int]) =
    val s = raw.split(":")(1).split("\\|").map(_.split(" ").filter(_.nonEmpty).map(_.toInt).toList)
    (s(0), s(1))

  private def calculatePoints(winning: List[Int], my: List[Int]): Int =
    Math.pow(2, my.count(winning.contains) - 1).toInt

  private def calculateCounts(winning: List[Int], my: List[Int]): Int = my.count(winning.contains)

  private val prepare = (input: List[String]) => input.map(toSeparated)

  private def sumCards(id: Int, cur: List[Int], touched: List[Int]): Int = touched.map(t => cur(t - 1)).sum + 1

  private def findTouched(id: Int, separated: List[List[Int]]): List[Int] =
    separated.zipWithIndex.filter(_._1.contains(id)).map(_._2 + 1)

  override def part1(input: List[String]): String = prepare(input).map(x => calculatePoints(x._1, x._2)).sum.toString
  override def part2(input: List[String]): String =
    val separated =
      prepare(input)
        .map(x => calculateCounts(x._1, x._2))
        .zipWithIndex
        .map(x => ((x._2 + 2) to List((x._2 + x._1 + 1), input.size).min).toList)
    var result = List.empty[Int]
    (1 to input.size).foreach(i =>
      val touched = findTouched(i, separated)
      val sum = sumCards(i, result, touched)
      result = result.appended(sum)
    )
    result.sum.toString
