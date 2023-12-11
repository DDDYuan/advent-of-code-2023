object Day9 extends GenericPuzzle("day9.csv"):
  private def toArray(raw: String) = raw.split(" ").map(_.toInt).toList
  private def calculateDifference(list: List[Int]) = list.zip(list.drop(1)).map(x => x._2 - x._1)
  private def predictNext(list: List[Int]): Int =
    if list.groupBy(i => i).keys.size == 1 then list.head else list.last + predictNext(calculateDifference(list))
  private def predictPrev(list: List[Int]): Int =
    if list.groupBy(i => i).keys.size == 1 then list.head else list.head - predictPrev(calculateDifference(list))
  override def part1(input: List[String]): String =
    input.map(toArray).map(predictNext).sum.toString
  override def part2(input: List[String]): String =
    input.map(toArray).map(predictPrev).sum.toString
