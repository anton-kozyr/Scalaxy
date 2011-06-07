import sbt._

class Plugins(info: ProjectInfo) extends PluginDefinition(info) {
  lazy val eclipsify = "de.element34" % "sbt-eclipsify" % "0.5.0"
  val sbtIdeaRepo = "sbt-idea-repo" at "http://mpeltonen.github.com/maven/"
  val sbtIdea = "com.github.mpeltonen" % "sbt-idea-plugin" % "0.4.0"
  val spde_sbt = "us.technically.spde" % "spde-sbt-plugin" % "0.4.2"
}

