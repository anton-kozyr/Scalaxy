package scalaxy

import scala.math._

object Tech extends Enumeration {
  type Tech = Value
  val Attack, Defence, Speed, Happiness = Value
}
import Tech._

case class Launch(from:Planet, to:Planet, race:Race)
case class Order(race:String, tech:Tech, launches:List[Launch]=Nil)

case class Techs(attack:Int=1, defence:Int=1, speed:Int=1, happiness:Int=1) {
  def increment(tech:Tech) = tech match {
    case Attack => copy(attack=attack+1)
    case Defence => copy(defence=defence+1)
    case Speed => copy(speed=speed+1)
    case Happiness => copy(happiness=happiness+1)
  }
}

case class Race(name:String, techs:Techs=Techs(), alive:Boolean=true)

case class Planet(id:Int=0, x:Float, y:Float, size:Int=0, pop:Int=0, race:String=null) {
  def acceptFleet(fleet:Fleet) = copy(pop=pop+fleet.size)

  def breed(r:Race, hunger:Int) = pop match {
    case 0 => this
    case pop if pop==size => this
    case pop if pop>size => // hunger
      copy(pop=size max pop-(r.techs.happiness*hunger))
    case _ => copy(pop=size min pop+r.techs.happiness)
  }
}

case class Fleet(size:Int, race:String, origId:Int, destId:Int, eta:Int) {
  def move = copy(eta=eta-1)
}

case class Scalaxy(size:Float, races:Set[Race], planets:Set[Planet],
                   fleets:Set[Fleet]=Set.empty, hunger:Int=2, maxETA:Int=3) {
  def adjust(z:Float):Float = z match {
    case _ if z < 0 => adjust(z+size)
    case _ if z < size => z
    case _ => adjust(z-size)
  }

  def distance(from:Planet, to:Planet) = {
    def delta(a:Float, b:Float) = abs(adjust(a) - adjust(b))
    sqrt(pow(delta(from.x, to.x), 2)+pow(delta(from.y, to.y), 2)).floatValue()
  }

  def fleetLaunch(from:Planet, to:Planet, r:Race) = from.copy(pop = ceil(from.pop/2.0).intValue()) match {
      case newFrom => newFrom -> Fleet(
        from.pop-newFrom.pop, from.race, from.id, to.id, ceil(distance(from, to)/r.techs.speed).intValue())
  }
}
