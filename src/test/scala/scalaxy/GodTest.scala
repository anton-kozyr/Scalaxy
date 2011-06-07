package scalaxy

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

class GodTest extends Spec with ShouldMatchers {
  describe ("Create galaxy") {
    val scalaxy = God.create(10, Set("ants", "bugs"), 10, 200, 100, 2, 3)
    println(scalaxy)
    scalaxy.planets should have size (10)
    scalaxy.planets.filter(_.race=="Barbarians") should have size (8)
  }
}