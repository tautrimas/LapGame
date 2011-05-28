import sbt._

class MyProject(info: ProjectInfo) extends DefaultProject(info) {
  
  override val mainClass = Some("lap.Main")
  
//   val scalatest = "org.scalatest" % "scalatest" % "1.2"
}