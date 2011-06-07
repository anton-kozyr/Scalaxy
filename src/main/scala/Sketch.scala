import processing.core._
import spde.core._
import PConstants._
import PApplet._

object graftRunner {
  def main(args: Array[String]) {
    PApplet.main(Array(classOf[`graft`].getName))
  }
}

class graft extends ProxiedApplet {
  lazy val px = new DrawProxy(this) {
    size(500, 200)
    frameRate(20)
    val items = {
      0 to width
    }.view.map {
      (_, random(255).toInt)
    }
    def draw {
      for ((x, color) <- items) {
        stroke(color)
        line(x, 0, x, height)
      }
    }

  }
}

/*
написать бота
запустить игру с ботами
доиграть
показать ходы
*/