object Day10 extends GenericPuzzle("day10.csv"):
  private enum Direction:
    case LEFT
    case RIGHT
    case UP
    case DOWN
  private def nextDirectionFrom(current: (Char, Direction)) = current match
    case ('-', Direction.LEFT)  => Direction.LEFT
    case ('-', Direction.RIGHT) => Direction.RIGHT
    case ('|', Direction.UP)    => Direction.UP
    case ('|', Direction.DOWN)  => Direction.DOWN
    case ('7', Direction.LEFT)  => Direction.UP
    case ('7', Direction.DOWN)  => Direction.RIGHT
    case ('L', Direction.UP)    => Direction.LEFT
    case ('L', Direction.RIGHT) => Direction.DOWN
    case ('J', Direction.LEFT)  => Direction.DOWN
    case ('J', Direction.UP)    => Direction.RIGHT
    case ('F', Direction.DOWN)  => Direction.LEFT
    case ('F', Direction.RIGHT) => Direction.UP
  private def getNextPosition(current: (Int, Int), from: Direction) = from match
    case Direction.LEFT  => (current._1, current._2 + 1)
    case Direction.RIGHT => (current._1, current._2 - 1)
    case Direction.UP    => (current._1 + 1, current._2)
    case Direction.DOWN  => (current._1 - 1, current._2)
  private def findStart(map: List[List[Char]]) =
    val x = map.zipWithIndex.find(_._1.contains('S')).get._2
    val y = map(x).indexOf('S')
    (x, y)
  private def findNext(current: ((Int, Int), Char, Direction), map: List[List[Char]]) =
    val nextFromDirection = nextDirectionFrom((current._2, current._3))
    val nextPosition = getNextPosition(current._1, nextFromDirection)
    val nextChar = map(nextPosition._1)(nextPosition._2)
    (nextPosition, nextChar, nextFromDirection)
  private def toMap(raw: List[String]) = raw.map(_.toCharArray.toList)
  private val START_BLOCK = 'F'
  private val START_FROM = Direction.DOWN

  private def getPath(input: List[String]) =
    val rawMap = toMap(input)
    val map = rawMap.map(_.map(x => if x == 'S' then START_BLOCK else x))
    val start = findStart(rawMap)
    var current = findNext((start, START_BLOCK, START_FROM), map)
    var path = List(current)
    while current._1 != start do
      current = findNext(current, map)
      path = path.appended(current)
    path
  override def part1(input: List[String]): String =
    val path = getPath(input)
    (path.size / 2).toString
  override def part2(input: List[String]): String =
    val path = getPath(input)
    ""