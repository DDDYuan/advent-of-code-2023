object Day10 extends GenericPuzzle("day10.csv"):
  enum Direction:
    case LEFT
    case RIGHT
    case UP
    case DOWN
  private possibleDirection = Map(
    'S' -> List(Direction.LEFT, Direction.RIGHT, Direction.UP, Direction.DOWN)
  )
  private def toMap(raw: List[String]) = raw.map(_.toCharArray.toList)
  override def part1(input:  List[String]): String = ""
  override def part2(input:  List[String]): String = ""

