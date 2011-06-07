import de.element34.sbteclipsify.SbtEclipsifyPlugin
import sbt._
import spde._

class Project(info: ProjectInfo) extends DefaultSpdeProject(info) with LocalLauncherProject with IdeaProject with SbtEclipsifyPlugin {
  val scalaToolsSnapshots = "Scala-Tools Maven2 Snapshots Repository" at "http://scala-tools.org/repo-snapshots"
  val scalatest = "org.scalatest" % "scalatest_2.8.1" % "1.5"
  val spde_core = "us.technically.spde" %% "spde-core" % spdeVersion.value
}

