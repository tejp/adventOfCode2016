object Solution extends App {
  val input = Common.getInput(3)

  def isNotTriangle(sides: Vector[Int]): Boolean = {
    val max = sides.max
    sides.sum - max <= max
  }

  //Part One
  val triangles = for {
    maybeTriangle <- input.split("\n").toVector.map(_.trim)
    sides = maybeTriangle.split(raw"\s+").toVector.map(_.toInt)
    if (!isNotTriangle(sides))
  } yield sides

  println("Part one: " + triangles.size)

  //Part Two
  val test = input.split(raw"\s+").withFilter(!_.isEmpty).map(_.toInt).toVector
  val triangles2 = for{
    group <- test.grouped(9)

    sides = Vector(one, two, three).map(_.toInt)
    if(!isNotTriangle(sides))
  } yield sides

  println("Part two: " + triangles2.size)
}