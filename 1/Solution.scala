object Solution extends App {

  val instructions = Common.getInput(1).trim.split(", ").toVector

  def getBlocks(instruction: String): Int =
    instruction.tail.toInt * (if (instruction(0) == 'R') -1 else 1)

  // Part 1
  val (_, finalPoint) = instructions.foldLeft(((false, true), (0, 0))) {
    case (((wasX, wasPositive), (lastX, lastY)), instruction) =>
      val diff = if (wasX == wasPositive) getBlocks(instruction) else -getBlocks(instruction)
      val newPoint = if (wasX) (lastX, lastY + diff) else (lastX + diff, lastY)
      ((!wasX, diff > 0), newPoint)
  }

  val p1 = math.abs(finalPoint._1) + math.abs(finalPoint._2)

  println(s"Part one: $p1")

  /*
  val TurnSteps = "^(.)(\\d*)$".r

  val sum = instructions.foldLeft(Vector(0, 0, 0, 0)) {
    case (rest :+ update, TurnSteps("L", blocks)) =>
      (update + blocks.toInt) +: rest
    case (old +: update +: rest, TurnSteps("R", blocks)) =>
      (update + blocks.toInt) +: rest :+ old
  }
  }*/


  // Part 2
  type Point = (Int, Int)
  private implicit class TupleOps2(self: Point) {
    def +(other: Point) = (self._1 + other._1, self._2 + other._2)
    def *(other: Point) = (self._1 * other._1, self._2 * other._2)
    def ==(other: Point) = self._1 == other._1 && self._2 == other._2
    def shift = (self._2, self._1)
  }

  case class State(
    pos: Point = (0, 0),
    dir: Point = (0, 1),
    visited: Vector[Point] = Vector.empty)

  val steps = for {
    instruction <- instructions
    turn = instruction(0)
    distance = instruction.tail.toInt
    steps <- turn +: Vector.fill(distance - 1)('F')
  } yield steps

  def findOverlap(state: State, steps: Vector[Char]): Point = {
    val nextDir = steps.head match {
      case 'F' => state.dir
      case 'R' => state.dir.shift * (1, -1)
      case 'L' => state.dir.shift * (-1, 1)
    }

    val nextPos = state.pos + nextDir
    state.visited.find(_ == nextPos).getOrElse{
      findOverlap(
        State(nextPos, nextDir, state.visited :+ state.pos),
        steps.tail)
    }
  }

  val bunnyHQ = findOverlap(State(), steps)
  val p2 = math.abs(bunnyHQ._1) + math.abs(bunnyHQ._2)
  println("Part two: " + p2)
}
