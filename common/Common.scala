object Common {
  def getInput(day: Int): String =
    scala.io.Source.fromFile(s"input/$day.txt", "UTF-8").mkString
}