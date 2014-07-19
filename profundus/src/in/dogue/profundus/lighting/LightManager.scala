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
  def addLights(ls:Seq[LightSource]) = {
    copy(lights = ls ++ lights)
  }
  private def process(cxy:Cell): Map[Cell, Double] = {
    val map = collection.mutable.Map[Cell, Double]().withDefaultValue(1)
    val cols = 32
    val rows = 32 + 16
    println("Cam" + cxy)
    for (l <- lights) {
      val (lx, ly) =  cxy |+| l.pos
      if (lx >= 0 && lx < cols && ly >= 0 && ly < rows) {
        for ((cell, d) <- l.fill) {
          map(cell) = (map(cell) - d).clamp(0, 1)
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
