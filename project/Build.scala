import sbt._
import Keys._
import scala.util.Try

object AdventOfCode2016 extends sbt.Build {

  val commonSettings = Seq(
    version := "1.0",
    scalaVersion := "2.12.0"
  )
  
  def root(base: File) =
    Project("adventOfCode2016", base, aggregate = toRef(base)).
      settings(commonSettings: _*)

  def commonModule(base: File) =
    Project("common", base / "common").
      settings(commonSettings: _*)

  val modules = (base: File) => {
    val common = commonModule(base)
    common +: (for {
      file <- base.listFiles.toSeq
      if file.isDirectory
      n <- Try(file.getName.toInt).
              toOption.
              filter { n => n > 0 && n < 26}
    } yield Project("u" + n, file).
              settings(commonSettings: _*).
              dependsOn(common))
  }

  val toRef = (base: File) =>
    modules(base).map { x => x: ProjectReference }
  override def projectDefinitions(baseDirectory: File): Seq[Project] =
    modules(baseDirectory) :+ root(baseDirectory)
}