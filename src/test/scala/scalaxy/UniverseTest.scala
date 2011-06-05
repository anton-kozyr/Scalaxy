package scalaxy

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

class UniverseTest extends Spec with ShouldMatchers {
  describe ("Create galaxy") {
    val scalaxy = Universe.generateScalaxy(10, Set("ants", "bugs"), 10, 200, 100, 2, 3)
    println(scalaxy)
    scalaxy.planets should have size (10)
    scalaxy.planets.filter(_.race=="Barbarians") should have size (8)
  }
}