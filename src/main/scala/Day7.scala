import scala.annotation.tailrec

object Day7 extends GenericPuzzle("day7.csv"):
  private def toStructured(raw: String) =
    val splitted = raw.split(" ")
    (splitted.head, splitted.last.toInt)
  private def isKindBiggerThan(kind: Char, other: Char): Boolean = kind match
    case 'A' => other != 'A'
    case 'K' => other != 'A' && other != 'K'
    case 'Q' => other != 'A' && other != 'K' && other != 'Q'
    case 'J' => other != 'A' && other != 'K' && other != 'Q' && other != 'J'
    case 'T' => other != 'A' && other != 'K' && other != 'Q' && other != 'J' && other != 'T'
    case _   => kind - other > 0
  private def isWildcardBiggerThan(kind: Char, other: Char): Boolean = kind match
    case 'A'               => other != 'A'
    case 'K'               => other != 'A' && other != 'K'
    case 'Q'               => other != 'A' && other != 'K' && other != 'Q'
    case 'T'               => other != 'A' && other != 'K' && other != 'Q' && other != 'T'
    case 'J'               => false
    case x if other == 'J' => x != 'J'
    case _                 => kind - other > 0
  private def toSimpleType(hand: String) = (
    hand.toCharArray
      .groupBy(i => i)
      .values
      .map(_.length)
      .toList
      .sorted
      .reverse,
    hand.toCharArray.toList
  )
  private def toWildCardType(hand: String) =
    val grouped = hand.toCharArray.groupBy(i => i).view.mapValues(_.length)
    val jCount = grouped.getOrElse('J', 0)
    val otherCount = grouped.toList.filter(_._1 != 'J').map(_._2).sorted.reverse
    (if otherCount.isEmpty then List(jCount) else otherCount.head + jCount :: otherCount.tail, hand.toCharArray.toList)
  @tailrec
  private def compareSimpleKinds(kind1: List[Char], kind2: List[Char], cf: (Char, Char) => Boolean): Boolean =
    (kind1, kind2) match
      case (k1, k2) if k1.size != k2.size             => false
      case (head1 :: _, head2 :: _) if head1 != head2 => cf(head1, head2)
      case (_ :: tail1, _ :: tail2)                   => compareSimpleKinds(tail1, tail2, cf)
      case _                                          => false
  @tailrec
  private def compareSimpleCounts(c1: List[Int], c2: List[Int]): Boolean = (c1, c2) match
    case (h1 :: _, h2 :: _) if h1 != h2 => h1 > h2
    case (_ :: t1, _ :: t2)             => compareSimpleCounts(t1, t2)
    case _                              => false
  private def compareSimpleHand(
    hand1: (List[Int], List[Char]),
    hand2: (List[Int], List[Char]),
    cf: (Char, Char) => Boolean
  ): Boolean =
    (hand1, hand2) match
      case ((c1, h1), (c2, h2)) if c1 == c2 => compareSimpleKinds(h1, h2, cf)
      case ((c1, _), (c2, _))               => compareSimpleCounts(c1, c2)
  private def isSimpleHandBiggerThanOther(hand: String, other: String): Boolean =
    compareSimpleHand(toSimpleType(hand), toSimpleType(other), isKindBiggerThan)
  private def isWildCardHandBiggerThanOther(hand: String, other: String): Boolean =
    compareSimpleHand(toWildCardType(hand), toWildCardType(other), isWildcardBiggerThan)
  private def calculatePoints(hands: List[Int]) = hands.reverse.zipWithIndex
    .map(x => x._1 * (x._2 + 1))
    .sum
    .toString
  override def part1(input: List[String]): String =
    calculatePoints(
      input
        .map(toStructured)
        .sortWith((hand, other) => isSimpleHandBiggerThanOther(hand._1, other._1))
        .map(_._2)
    )
  override def part2(input: List[String]): String =
    calculatePoints(
      input
        .map(toStructured)
        .sortWith((hand, other) => isWildCardHandBiggerThanOther(hand._1, other._1))
        .map(_._2)
    )
