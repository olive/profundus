package in.dogue.profundus.lighting

import in.dogue.antiqua.Antiqua._
import in.dogue.antiqua.graphics.{Tile, Filter}
import com.deweyvm.gleany.data.Recti
import in.dogue.antiqua.geometry.Circle

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
  private def process(cxy:Cell): Map[Cell, Double] = {
    val map = collection.mutable.Map[Cell, Double]().withDefaultValue(1)
    val cols = 32
    val rows = 32 + 16
    val screenRect = Recti(0, 0, cols, rows)
    for (l <- lights) {
      val c = l.isOnScreen(cxy, screenRect)
      if (c) {
        for ((cell, d) <- l.fill) {
          val c = cell |+| cxy
          map(c) = (map(c) - d).clamp(0, 1)
        }
      }
    }
    map.toMap.withDefaultValue(1)
  }

  def getFilter(cxy:Cell):Cell => Tile => Tile = {
    val mp = process(cxy)
    def dark(c:Cell)(t:Tile):Tile = {
      val dimAmt = 1 - mp(c)
      t.mapFg(_.mult(dimAmt)).mapBg(_.mult(dimAmt))
    }
    dark
  }
}
