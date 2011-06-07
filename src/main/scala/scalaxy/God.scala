package scalaxy

object God {
  def create(size:Int, raceNames:Set[String], planetsCount:Int, maxPlanetSize:Int, hwSize:Int, hunger:Int, maxETA:Int) = {
    lazy val rand = scala.util.Random
    lazy val sz:Float = size
    lazy val barbs = Race("Barbarians")
    val rf:Float => Float = rand.nextFloat * _
    val rt:Int => Int = rand.nextInt(_) + 1
    def planet(id:Int, r:Race=barbs) = {
      val s = if(r==barbs) rt(maxPlanetSize) else hwSize
      Planet(id, rf(sz), rf(sz), s, s, r.name)
    }
    val races = raceNames.map(Race(_))
    val planets = List.range(0, planetsCount).foldLeft((Set[Planet](), races.toList))((t, i) => t._2 match {
      case Nil => (t._1 + planet(i), Nil)
      case h :: tail => (t._1 + planet(i, h), tail)
    })._1
    Scalaxy(size, races + barbs, planets, Set.empty, hunger, maxETA)
  }
}