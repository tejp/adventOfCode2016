object Solution extends App {
  val instructions = Common.getInput(2).trim.split("\n").toVector

  // Part 1
  def nextButton(moves: String): Int = moves.foldLeft(12){ (button, move) =>
    button + (move match {
      case 'U' => if (button > 10) -10 else 0
      case 'D' => if (button < 20) 10 else 0
      case 'R' => if (button % 10 < 3) 1 else 0
      case 'L' => if (button % 10 > 1) -1 else 0
    })
  }

  val keyPresses = for{
    instruction <- instructions
    button = nextButton(instruction)
    number = (button / 10) * 3 + button % 10
  } yield number

  println("Part one: " + keyPresses.mkString)

  //Part 2
  val nextButton = Map(
    '1' -> Map('D' -> '3'),
    '2' -> Map('D' -> '6', 'R' -> '3'),
    '3' -> Map('L' -> '2', 'U' -> '1', 'D' -> '7', 'R' -> '4'),
    '4' -> Map('L' -> '3', 'D' -> '8'),
    '5' -> Map('R' -> '6'),
    '6' -> Map('L' -> '5', 'U' -> '2', 'D' -> 'A', 'R' -> '7'),
    '7' -> Map('L' -> '6', 'U' -> '3', 'D' -> 'B', 'R' -> '8'),
    '8' -> Map('L' -> '7', 'U' -> '4', 'D' -> 'C', 'R' -> '9'),
    '9' -> Map('L' -> '8'),
    'A' -> Map('U' -> '6', 'R' -> 'B'),
    'B' -> Map('L' -> 'A', 'U' -> '7', 'D' -> 'D', 'R' -> 'C'),
    'C' -> Map('L' -> 'B', 'U' -> '8'),
    'D' -> Map('U' -> 'B')
  )

  val code = instructions.map(_.foldLeft('5') {
    (current, inst) => nextButton(current).get(inst).getOrElse(current)
  })

  println("Part two: " + code.mkString)
}