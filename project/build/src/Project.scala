import sbt._
import de.element34.sbteclipsify._

class Project(info: ProjectInfo) extends DefaultProject(info)  with IdeaProject with SbtEclipsifyPlugin {
  val scalaToolsSnapshots = "Scala-Tools Maven2 Snapshots Repository" at "http://scala-tools.org/repo-snapshots"
  val scalatest = "org.scalatest" % "scalatest_2.8.1" % "1.5"
}

