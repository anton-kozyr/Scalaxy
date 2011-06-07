package scalaxy

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec

class ScalaxyTest extends Spec with ShouldMatchers {
  describe ("Distance between planets") {
    val scalaxy = new Scalaxy(10, Set.empty, Set.empty, Set.empty);
    it ("should return zero") {
      scalaxy.distance(
        Planet(1, 1, 1, 1, 1, null),
        Planet(2, 1, 1, 1, 1, null)) should be (0)
      scalaxy.distance(
        Planet(1, 1, 1, 1, 1, null),
        Planet(2, 11, 11, 1, 1, null)) should be (0)
    }

    it ("should return real distance") {
      scalaxy.distance(
        Planet(1, 0, 3, 1, 1, null),
        Planet(2, 4, 0, 1, 1, null)) should be (5)
      scalaxy.distance(
        Planet(1, 0, 13, 1, 1, null),
        Planet(2, -6, 0, 1, 1, null)) should be (5)
    }
  }

  describe ("Improve techs") {
    import Tech._
    it ("should improve attack") {
      Techs(1, 1, 1, 1).increment(Attack) should be (Techs(2, 1, 1, 1))
    }
    it ("should improve defence") {
      Techs(11, 1, 1, 1).increment(Defence) should be (Techs(11, 2, 1, 1))
    }
    it ("should improve speed") {
      Techs(1, 1, 1, 2).increment(Speed) should be (Techs(1, 1, 2, 2))
    }
    it ("should improve happiness") {
      Techs(1, 1, 1, 2).increment(Happiness) should be (Techs(1, 1, 1, 3))
    }
  }

  describe ("Single fleet launch") {
    val r = Race("A")
    val p1 = Planet(0, x=1, y=1.1.floatValue(), pop=0)
    val p2 = Planet(1, x=1, y=1, pop=10, race=r.name)
    val p3 = Planet(2, x=2, y=2, pop=0)
    val scalaxy = Scalaxy(10, Set(r), Set.empty)

    it ("fleet should be launched, population should be halved, eta calculated") {
      scalaxy.fleetLaunch(p2, p3, r) should be ((p2.copy(pop=5), Fleet(5, "A", 1, 2, 2)))
    }

    it ("eta should be at least 1") {
      scalaxy.fleetLaunch(p2, p1, r) should be ((p2.copy(pop=5), Fleet(5, "A", 1, 0, 1)))
    }
  }

  describe ("Breeding") {
    val p1 = Planet(0, 0, 0, size=10, pop=1, "A")
    val p2 = Planet(1, 0, 0, size=10, pop=8, "A")
    val p3 = Planet(2, 0, 0, size=10, pop=10, "A")
    val p4 = Planet(3, 0, 0, size=10, pop=0)
    val p5 = Planet(3, 0, 0, size=10, pop=12, "A")
    val p6 = Planet(3, 0, 0, size=10, pop=30, "A")
    val r: Race = Race("A", Techs(happiness = 5))

    it ("population should grow") {
      p1.breed(r, 2) should be (p1.copy(pop=6))
      p2.breed(r, 2) should be (p2.copy(pop=10))
    }

    it ("population shouldn't grow") {
      p3.breed(r, 2) should be (p3.copy(pop=10))
      p4.breed(r, 2) should be (p4.copy(pop=0))
    }

    it ("population should decrease") {
      p5.breed(r, 2) should be (p5.copy(pop=10))
      p6.breed(r, 2) should be (p6.copy(pop=20))
    }
  }

  describe ("Extracts launches from orders") {
    val pa1 = Planet(0, x=0, y=0, size=10, pop=1, "ants")
    val pa2 = Planet(1, x=2, y=0, size=10, pop=1, "ants")
    val pb1 = Planet(2, x=4, y=0, size=10, pop=1, "bugs")
    val pb2 = Planet(3, x=1, y=0, size=10, pop=1, "bugs")
    val ants: Race = Race("ants")
    val bugs: Race = Race("bugs", Techs(speed = 2))
    val scalaxy = Scalaxy(10, Set(ants, bugs), Set(pa1, pa2, pb1, pb2))

    it ("should return an empty map (origin==dest)") {
      val aOrder = Order("ants", Tech.Speed, List(Launch(pa1, pa1, ants)))
      val bOrder = Order("bugs", Tech.Speed, List())
      Universe.extractLaunches(scalaxy, Set(aOrder, bOrder)) should be (Map[Planet, List[Launch]]())
    }

    it ("should filter out launches beyond maxETA") {
      val aOrder = Order("ants", Tech.Speed, List(Launch(pa1, pb1, ants)))
      Universe.extractLaunches(scalaxy, Set(aOrder)) should be (Map[Planet, List[Launch]]())
    }

    it ("should filter out launches from enemy planet") {
      val aOrder = Order("ants", Tech.Speed, List(Launch(pb1, pb2, ants)))
      Universe.extractLaunches(scalaxy, Set(aOrder)) should be (Map[Planet, List[Launch]]())
    }

    it ("should return valid launches (all valid)") {
      val la1 = Launch(pa1, pa2, ants)
      val la3 = Launch(pa1, pb2, ants)
      val la5 = Launch(pa2, pb1, ants)
      val lb1 = Launch(pb1, pa1, bugs)
      val lb2 = Launch(pb1, pa2, bugs)
      val aOrder = Order("ants", Tech.Speed, List(la1, la3, la5))
      val bOrder = Order("bugs", Tech.Speed, List(lb1, lb2))
      Universe.extractLaunches(scalaxy, Set(aOrder, bOrder)) should be (Map[Planet, List[Launch]](
        (pa1 -> List(la1, la3)), (pa2 -> List(la5)), (pb1 -> List(lb1, lb2)) ))
    }

    it ("should filter out invalid launches, but leave valid ones (summary)") {
      val la1 = Launch(pa1, pa2, ants)
      val la2 = Launch(pa1, pb1, ants)
      val la3 = Launch(pa1, pb2, ants)
      val la4 = Launch(pb1, pb2, bugs)
      val la5 = Launch(pa2, pb1, ants)
      val lb1 = Launch(pb1, pa1, bugs)
      val lb2 = Launch(pb1, pa2, bugs)
      val aOrder = Order("ants", Tech.Speed, List(la1, la2, la3, la4, la5))
      val bOrder = Order("bugs", Tech.Speed, List(lb1, lb2))

      Universe.extractLaunches(scalaxy, Set(aOrder, bOrder)) should be (Map[Planet, List[Launch]](
        (pa1 -> List(la1, la3)), (pa2 -> List(la5)), (pb1 -> List(lb1, lb2)) ))
    }
  }

  describe ("Launch fleets") {
    val pa1 = Planet(0, x=0, y=0, size=10, pop=8, "ants")
    val pa2 = Planet(1, x=2, y=0, size=10, pop=8, "ants")
    val pb1 = Planet(2, x=4, y=0, size=10, pop=8, "bugs")
    val pb2 = Planet(3, x=1, y=0, size=10, pop=6, "bugs")
    val pb3 = Planet(4, x=1, y=1, size=10, pop=8, "bugs")
    val ants: Race = Race("ants")
    val bugs: Race = Race("bugs", Techs(speed = 2))
    val scalaxy = Scalaxy(10, Set(ants, bugs), Set(pa1, pa2, pb1, pb2, pb3))

    it ("should join several fleets with the same origin and destination into one") {
      val (planets, fleets) = Universe.launchAllFleets(scalaxy, Map[Planet, List[Launch]](
        (pb1 -> List(
          Launch(pb1, pa1, bugs),
          Launch(pb1, pa1, bugs),
          Launch(pb1, pa1, bugs)))))

      fleets should be (Set(Fleet(7, "bugs", 2, 0, 2)))
    }

    it ("should exhaust planet but create only 3 fleets not 4") {
      val (planets, fleets) = Universe.launchAllFleets(scalaxy, Map[Planet, List[Launch]](
        (pb2 -> List(
          Launch(pb2, pa2, bugs),
          Launch(pb2, pa1, bugs),
          Launch(pb2, pb1, bugs),
          Launch(pb2, pb3, bugs)))))

      planets should be (Set(
        Planet(0, x=0, y=0, size=10, pop=8, "ants"),
        Planet(1, x=2, y=0, size=10, pop=8, "ants"),
        Planet(2, x=4, y=0, size=10, pop=8, "bugs"),
        Planet(3, x=1, y=0, size=10, pop=1, "bugs"),
        Planet(4, x=1, y=1, size=10, pop=8, "bugs")
      ))

      fleets should be (Set(
        Fleet(1, "bugs", 3, 0, 1),
        Fleet(3, "bugs", 3, 1, 1),
        Fleet(1, "bugs", 3, 2, 2)
      ))
    }

    it ("should change population and create fleets (summary)") {
      val (planets, fleets) = Universe.launchAllFleets(scalaxy, Map[Planet, List[Launch]](
        (pa1 -> List(Launch(pa1, pa2, ants), Launch(pa1, pb2, ants))),
        (pa2 -> List(Launch(pa2, pb1, ants))),
        (pb1 -> List(Launch(pb1, pa1, bugs), Launch(pb1, pa2, bugs)))))

      planets should be (Set(
        Planet(0, x=0, y=0, size=10, pop=2, "ants"),
        Planet(1, x=2, y=0, size=10, pop=4, "ants"),
        Planet(2, x=4, y=0, size=10, pop=2, "bugs"),
        Planet(3, x=1, y=0, size=10, pop=6, "bugs"),
        Planet(4, x=1, y=1, size=10, pop=8, "bugs")
      ))

      fleets should be (Set(
        Fleet(4, "ants", 0, 1, 2),
        Fleet(2, "ants", 0, 3, 1),
        Fleet(4, "ants", 1, 2, 2),
        Fleet(4, "bugs", 2, 0, 2),
        Fleet(2, "bugs", 2, 1, 1)
      ))
    }
  }

  describe ("Move, land and unload fleets") {
    val p1 = Planet(0, x=2, y=0, size=10, pop=8, "ants")
    val p2 = Planet(1, x=4, y=0, size=10, pop=8, "bugs")
    val landing = Fleet(5, "ants", -1, 0, 1)
    val flying1 = Fleet(5, "ants", -1, 0, 2)
    val flying2 = Fleet(5, "ants", -1, 1, 2)
    val attacking = Fleet(5, "ants", -1, 1, 1)
    val planets = Set(p1, p2)
    val scalaxy = Scalaxy(10, Set(Race("ants"), Race("bugs")), planets)

    it ("should land a fleet and unload it adding to population") {
      Universe.moveAndUnloadFleets(planets, Set(landing)) should be (
        (Set(p2, p1.copy(pop=8+5)), Set.empty, Set.empty))
    }

    it ("should move fleet") {
      Universe.moveAndUnloadFleets(planets, Set(flying1, flying2)) should be (
        (planets, Set(flying1.copy(eta = 1), flying2.copy(eta = 1)), Set.empty))
    }

    it ("should move fleet, but not land on enemy planet") {
      Universe.moveAndUnloadFleets(planets, Set(attacking)) should be (
        (planets, Set.empty, Set(attacking.copy(eta=0))))
    }
  }

  describe ("Attack a planet by one fleet") {
    it ("should wipe old race and replace with new") {
      Universe.attack(
        Planet(0, 0, 0, 100, 100, "loosers"), Fleet(51, "winners", -1, 0, 0),
        Race("loosers"), Race("winners", Techs(attack=2))) should be (Planet(0, 0, 0, 100, 1, "winners"))
    }

    it ("should wipe old race with strong defence") {
      Universe.attack(
        Planet(0, 0, 0, 10, 1, "loosers"), Fleet(6, "winners", -1, 0, 0),
        Race("loosers", Techs(defence=2)), Race("winners")) should be (Planet(0, 0, 0, 10, 4, "winners"))
    }

    it ("should wipe old race but won't capture planet") {
      Universe.attack(
        Planet(0, 0, 0, 100, 100, "survivors"), Fleet(100, "aggressors", -1, 0, 0),
        Race("survivors"), Race("aggressors")) should be (Planet(0, 0, 0, 100, 0, "survivors"))
    }

    it ("should leave some old population") {
      Universe.attack(
        Planet(0, 0, 0, 100, 100, "winners"), Fleet(49, "loosers", -1, 0, 0),
        Race("loosers"), Race("winners", Techs(attack=2))) should be (Planet(0, 0, 0, 100, 2, "winners"))
    }
  }

  describe ("Attack planets") {
    val ap = Planet(0, x=0, y=0, size=10, pop=10, "ants")
    val bp = Planet(1, x=0, y=0, size=10, pop=10, "bugs")
    val cp = Planet(2, x=0, y=0, size=10, pop=10, "cats")
    // all fleets are attacking ap(id=0), default size is zero
    val bf0 = Fleet(0, "bugs", 0, 0, 0)
    val bf1 = Fleet(0, "bugs", 0, 0, 0)
    val cf0 = Fleet(0, "cats", 0, 0, 0)
    val planets = Set(ap, bp, cp)
    val ants = Race("ants")
    val bugs = Race("bugs")
    val cats = Race("cats")
    val races = Set(ants, bugs, cats)

    it ("should return planets in old state - no one is attacking") {
      Universe.attackAll(planets, Set.empty, races) should be (planets)
    }

    it ("should wipe old race and replace with new") {
      Universe.attackAll(
        Set(ap), 
        Set(bf0.copy(size=5, destId=0)), 
        Set(ants, bugs.copy(techs=Techs(attack=3)))
      ) should be (Set(ap.copy(pop=1, race="bugs")))
    }

    it ("should leave old planet's owner") {
      Universe.attackAll(
        Set(ap), Set(bf0.copy(size=5, destId=0)), races
      ) should be (Set(ap.copy(pop=5, race="ants")))

      Universe.attackAll(
        Set(ap), Set(bf0.copy(size=10, destId=0)), races
      ) should be (Set(ap.copy(pop=0, race="ants")))
    }

    it ("two fleets of one race attack a planet") {
      Universe.attackAll(
        Set(ap), 
        Set(bf0.copy(size=5), bf1.copy(size=6)), 
        Set(ants, bugs)
      ) should be (Set(ap.copy(pop=1, race="bugs")))

      Universe.attackAll(
        Set(ap), 
        Set(bf0.copy(size=6), bf1.copy(size=5)), 
        Set(ants, bugs.copy(techs=Techs(attack=2)))
      ) should be (Set(ap.copy(pop=(6), race="bugs")))
    }

    it ("two fleets of different races attack a planet. bugs always attack first") {
      Universe.attackAll(
        Set(ap),
        Set(bf0.copy(size=5), cf0.copy(size=6)), 
        races,
        (_.race < _.race) // force bugs to be first attacker
      ) should be (Set(ap.copy(pop=1, race="cats")))

      Universe.attackAll(
        Set(ap), 
        Set(bf0.copy(size=11), cf0.copy(size=6)), 
        Set(ants, bugs.copy(techs=Techs(defence=2)), cats),
        (_.race < _.race) // force bugs to be first attacker
      ) should be (Set(ap.copy(pop=4, race="cats")))
    }
  }

  describe ("Process galaxy") {
    val scalaxy = new Scalaxy(10, Set.empty, Set.empty, Set.empty);
    it ("processes empty galaxy for empty orders") {
      Universe.processOrders(scalaxy, Set.empty) should be (scalaxy)
    }
  }
}
