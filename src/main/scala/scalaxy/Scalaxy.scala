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
case class Planet(id:Int=0, x:Float, y:Float, size:Int=0, pop:Int=0, race:String=null)
case class Fleet(size:Int, race:String, origId:Int, destId:Int, eta:Int)

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

  def fleetMove(fleet:Fleet) = fleet.copy(eta=fleet.eta-1)

  def fleetLand(fleet:Fleet, p:Planet) = p.copy(pop=p.pop+fleet.size)

  def planetBreed(p:Planet, r:Race) = p.pop match {
    case 0 => p
    case pop if pop==p.size => p
    case pop if pop>p.size => // hunger
      p.copy(pop=max(p.size, p.pop-(r.techs.happiness*hunger)))
    case _ => p.copy(pop=min(p.size, p.pop+r.techs.happiness))
  }

  /**
   * Attack planet(dp) of race(dr) by fleet(af) of race(af).
   * Returns planet.
   */
  def attack(dp:Planet, af:Fleet, dr:Race, ar:Race) = {
    val attackResult:Int = af.size * ar.techs.attack - dp.pop * dr.techs.defence
    if (attackResult > 0) 
      dp.copy(pop=attackResult/ar.techs.attack, race=ar.name);
    else
      dp.copy(pop= -attackResult/dr.techs.defence);
  }

  val random = scala.util.Random

  /**
   * Returns a map of launches per planet .
   * Launch is considered valid if and only if: <br/>
   * 1. it was initiated by a race from a planet owned by the race. <br/>
   * 2. initial ETA is less or equal to maxETA <br/>
   * 3. origin != destination
   *
   */
  def extractLaunches(orders:Set[Order]) = {
    def valid(launch: Launch, raceName: String) =
      (launch.race.name == raceName) &&
      (launch.from.race == raceName) &&
      (launch.from != launch.to) &&
      (distance(launch.from, launch.to) <= launch.race.techs.speed * maxETA)
    orders.foldLeft(List[(Planet,Launch)]())(
        (lst, ord) => lst ::: ord.launches.filter(lnch => valid(lnch, ord.race))
        .map(lnch => (lnch.from, lnch))) // returns List of tuples (Planet -> Launch)
      .groupBy(_._1) // group by planet (maping of planet to List of tuples)
      .map({ case (p, l) => (p, l.map(_._2))}) // mapping of planet to Lis of Launches (reduced from tuples)
  }

  /**
   * Executes all launches and returns list of planets and new fleets
   */
  def launchAllFleets(launches:Map[Planet, List[Launch]]) = {
    // for a planet and list of launches creates tuple of the planet and all fleets launched from it
    def launchAll(planet:Planet, launches:List[Launch]):(Planet, Set[Fleet]) = launches match {
      case Nil => (planet, Set.empty) // stop if no launches left
      case hd :: tail => planet.pop match {
        case pop if pop <= 1 => (planet, Set.empty) // stop if population 1
        case _ => fleetLaunch(planet, hd.to, hd.race) match { // launch fleet from hd
          case (pla, fl) => launchAll(pla, tail) match { // recursion on updated planet and reminding list of launchers
            case (pl, fleets) => (pl, joinOrAddFleet(fleets, fl)) // merge results
          }
        }
      }
    }

    // Adds a fleet to the list or join with existing one
    def joinOrAddFleet(fleets:Set[Fleet], nf:Fleet) = {
      var joined = false
      val result = fleets.foldLeft(Set[Fleet]())((fleets, f) => (joined, f) match {
        case (false, Fleet(_, nf.race, nf.origId, nf.destId, nf.eta)) => {
          joined = true
          fleets + f.copy(size=f.size+nf.size)
        }
        case _ => fleets + f
      })
      if (joined) result
      else result + nf
    }

    // for each planet, apply launches (if exist) and return two lists: planet and new fleets
    planets.map(p => launchAll(p, launches.get(p).getOrElse(Nil)))
      .foldLeft((Set[Planet](), Set[Fleet]()))((res, pf) => (res._1 + pf._1, res._2 ++ pf._2))

  }

  /**
   * Moves all fleets, then unloads them if possible.
   * Returns tuple of (updated planets, moving fleets, attacking fleets)
   */
  def moveAndUnloadFleets(planets:Set[Planet], fleets:Set[Fleet]) = {
    val pm = scala.collection.mutable.HashMap[Int, Planet]()
    planets.foreach(p => pm += (p.id -> p))
    val (moving, attacking) = fleets.map(fleetMove).foldLeft((Set[Fleet](), Set[Fleet]()))((ft, f) =>
      (f.eta, f.race == pm(f.destId).race) match {
        case (0, true) => {
          pm += (f.destId -> fleetLand(f, pm(f.destId)))
          ft
        }
        case (0, false) => (ft._1, ft._2 + f)
        case _ => (ft._1 + f, ft._2)
      })
    (pm.map(_._2).toSet, moving, attacking)
  }

  /**
   * Goes through list of all attacking fleets in SPECIFIED order (random for gameplay, and sorted by (planet,raceName) in testing).
   * Fleets are attacking, unloading where possible and attacking again until none left.
   * Returns set of planets
   */
   def attackAll(planets:Set[Planet], fleets:Set[Fleet], races:Set[Race], 
     fleetsSorter: (Fleet, Fleet) => Boolean = {(_,_) => random.nextBoolean}) = {
     // map: raceName -> race
     val rm = races.map(r => (r.name -> r)).toMap
     // map: planetId -> (planet, race)
     val pm = planets.map(p => (p.id -> p)).toMap
     // map: planetId -> sortedList((fleet, race))
     val fm = fleets.toList.sortWith(fleetsSorter).map(f => (f, rm(f.race))).groupBy(_._1.destId)

     // processFleets(planet, race, list(fleet -> race)) returns planet
     def processFleets(planet:Planet, fts:List[(Fleet, Race)]) = {
       fts.foldLeft(planet)((p, fa) => if(p.race==fa._1.race) {
           fleetLand(fa._1, p)
         } else {
           attack(p, fa._1, rm(p.race), fa._2)
         })
     }
     pm.foldLeft(Set[Planet]())((s, el) => s + processFleets(el._2, fm.get(el._1).getOrElse(Nil)))
   }

   /**
    * Process population on all planets. Mark races as dead if they don't have planets anymore
    */
   def breedAll(planets:Set[Planet], races:Set[Race]) = {
     // map: raceName -> race
     val rm = races.map(r => (r.name -> r.copy(alive=false))).toMap
     planets.foldLeft((Set[Planet](), Set[Race]()))((t, p) => (t._1 + planetBreed(p, rm(p.race)), t._2 + rm(p.race).copy(alive=true)))
   }

   /**
    * Improve technologies for all alive races
    */
   def improveTechs(orders:Set[Order], races:Set[Race]) = {
     // map: race -> tech
     val tm = orders.map(o => (o.race -> o.tech)).toMap
     races.map(r => if(r.alive) r.copy(techs=r.techs.increment(tm.get(r.name).getOrElse(Defence))) else r)
   }

   def processOrders(orders:Set[Order]) = {
     val launches = extractLaunches(orders)
     val (planetsAfterLaunch, freshFleets) = launchAllFleets(launches)
     val (planetsAfterLanding, flyingFleets, attackingFleets) = moveAndUnloadFleets(planetsAfterLaunch, fleets ++ freshFleets) 
     val planetsAfterAttack = attackAll(planetsAfterLanding, attackingFleets, races)
     val (planetsAfterBreeding, racesAfterDying) = breedAll(planetsAfterAttack, races)
     val racesAfterResearch = improveTechs(orders, racesAfterDying)
     Scalaxy(size, racesAfterResearch, planetsAfterBreeding, flyingFleets, hunger, maxETA)
   }
}

