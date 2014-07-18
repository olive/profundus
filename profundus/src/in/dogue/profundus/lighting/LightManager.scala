package in.dogue.profundus.lighting

import in.dogue.antiqua.Antiqua._
import in.dogue.antiqua.graphics.{Tile, Filter}

object LightManager {
  def create = LightManager(Seq())
}

case class LightManager(lights:Seq[LightSource]) {
  def addLight(ls:LightSource) = {
    copy(lights = ls +: lights)
  }
  private def process: Map[Cell, Double] = {
    val map = collection.mutable.Map[Cell, Double]().withDefaultValue(1)
    for (l <- lights) {
      for ((cell, d) <- l.fill) {
        map(cell) = (map(cell) - d).clamp(0,1)
      }
    }
    map.toMap.withDefaultValue(1)
  }

  def getFilter:Cell => Tile => Tile = {
    val mp = process
    def dark(c:Cell)(t:Tile):Tile = {
      val dimAmt = 1 - mp(c)
      t.mapFg(_.mult(dimAmt)).mapBg(_.mult(dimAmt))
    }
    dark
  }
}
