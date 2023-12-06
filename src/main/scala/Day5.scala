import scala.annotation.tailrec
import scala.collection.immutable.NumericRange

object Day5 extends GenericPuzzle("day5.csv"):
  private def getSeeds(firstLine: String): List[BigInt] =
    firstLine
      .split(": ")(1)
      .split(" ")
      .map(BigInt.apply)
      .toList
  private def getSeedsRange(firstLine: String) =
    val seeds = getSeeds(firstLine)
    var ranges = List.empty[NumericRange.Exclusive[BigInt]]
    for
      i <- 0 until (seeds.size, 2)
      range = seeds(i) until seeds(i) + seeds(i + 1)
    yield range
  private def getRangeMapping(raw: String): (NumericRange.Exclusive[BigInt], BigInt => BigInt) =
    val splitted = raw.split(" ")
    val rangeStart = BigInt(splitted(0))
    val range = rangeStart until rangeStart + BigInt(splitted(2))
    val prev = (number: BigInt) => number - rangeStart + BigInt(splitted(1))
    (range, prev)
  private def toMappings(raw: List[String]) = raw.drop(1).map(getRangeMapping)
  private def toPrev(number: BigInt, mappings: List[(NumericRange.Exclusive[BigInt], BigInt => BigInt)]): BigInt =
    mappings.find(_._1.contains(number)).map(_._2(number)).getOrElse(number)
  private def toMaps(raw: List[String]) =
    val rawMaps = raw.drop(2)
    var cur = List.empty[String]
    var maps = List.empty[List[String]]
    rawMaps.foreach(line =>
      if line.isEmpty then
        maps = maps.appended(cur)
        cur = Nil
      else cur = cur.appended(line)
    )
    if cur.nonEmpty then maps = maps.appended(cur)
    maps.map(toMappings).reverse
  private def toSeed(location: BigInt, maps: List[List[(NumericRange.Exclusive[BigInt], BigInt => BigInt)]]): BigInt =
    var result = location
    maps.foreach(m =>
      val prev = toPrev(result, m)
      result = prev
    )
    result
  @tailrec
  private def findSeed(
    location: BigInt,
    maps: List[List[(NumericRange.Exclusive[BigInt], BigInt => BigInt)]],
    seeds: List[NumericRange.Exclusive[BigInt]]
  ): BigInt =
    val seed = toSeed(location, maps)
    if seeds.exists(_.contains(seed)) then location else findSeed(location + 1, maps, seeds)
  override def part1(input: List[String]): String =
    val maps = toMaps(input)
    val seeds = getSeeds(input.head)
    findSeed(0, maps, seeds.map(x => x until x + 1)).toString
  override def part2(input: List[String]): String =
    val maps = toMaps(input)
    val seeds = getSeedsRange(input.head).toList
    findSeed(0, maps, seeds).toString
