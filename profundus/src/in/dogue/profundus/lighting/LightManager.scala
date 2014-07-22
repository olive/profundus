package in.dogue.profundus.lighting

import in.dogue.antiqua.Antiqua._
import in.dogue.antiqua.graphics.{Tile, Filter}
import com.deweyvm.gleany.data.Recti
import in.dogue.antiqua.geometry.Circle
import in.dogue.antiqua.data.Array2d
import in.dogue.profundus.Game

object LightManager {
  def create(cols:Int, rows:Int) = LightManager(cols, rows, Seq())
}

case class LightManager(cols:Int, rows:Int, lights:Seq[LightSource]) {
  def reset = LightManager.create(cols, rows)
  def addLight(ls:LightSource) = {
    copy(lights = ls +: lights)
  }
  def addLights(ls:Seq[LightSource]) = {
    copy(lights = ls ++ lights)
  }



  private def process(cxy:Cell) = Game.drawPerf.track("processLights") {
    val map = new Array[Double](cols*rows)

    val screenRect = Recti(0, 0, cols, rows)
    lights.par.filter( l => l.isOnScreen(cxy, screenRect)).map { l =>
      l.fill.map { case (cell, d) =>
        val c = cell |+| cxy
        if (screenRect.contains(c)) {
          val k = Array2d.coordsToIndex(c, cols)
          map(k) += d
        }
      }

    }

    for (k <- 0 until (cols * rows)) {
      map(k) = map(k).clamp(0, 1)
    }
    map
  }

  def getFilter(cxy:Cell):Cell => Tile => Tile = {
    val mp = process(cxy)
    def dark(c:Cell)(t:Tile):Tile = {
      val k = Array2d.coordsToIndex(c, cols)
      val dimAmt = mp(k)
      t.mapFg(_.mult(dimAmt)).mapBg(_.mult(dimAmt))
    }
    dark
  }
}
