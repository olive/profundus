package in.dogue.profundus.world.features

import in.dogue.antiqua.Antiqua.Cell
import in.dogue.profundus.world.{Feature, WorldTile, TerrainScheme}
import in.dogue.antiqua.data.Array2d
import scala.util.Random
import in.dogue.profundus.doodads.Campfire
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.antiqua.geometry.{Line, Circle}
import com.deweyvm.gleany.data.Recti

case class Campsite(center:Cell, radius:Int) {
  def placeSite(cols:Int, rows:Int, yy:Int, scheme:TerrainScheme, terrain:Array2d[WorldTile], r:Random) = {
    val fillDepth = center +| (radius/2)
    val circle = Circle(center, radius)
    val line = Line.bresenham(center.x, center.y, center.x + radius, center.y)
    val line2 = Line.bresenham(center.x, center.y, center.x - radius, center.y)
    val newTiles = terrain.map { case (i, j, t) =>
      val p = (i, j)
      val contains = circle.contains(p)
      if (line.contains(p) || line2.contains(p)) {
        WorldTile(scheme.makeEmpty(r))
      } else if (contains && j > fillDepth.y) {
        WorldTile(scheme.makeDirt(r))
      } else if (contains && (center |-| p).mag > 3*radius/4){
        WorldTile(scheme.makeDirt(r))
      } else if (contains) {
        WorldTile(scheme.makeEmpty(r))
      } else {
        t
      }


    }
    val siteSpot = center +| yy +| radius/2
    (newTiles, Seq(Campfire.create(siteSpot).toDoodad), Seq())
  }
  def toFeature(cols:Int, rows:Int):Feature = {
    val rect = Recti(center.x - radius/2, center.y - radius/2, radius*2, radius*2)
    val f = placeSite _
    Feature(rect, f)
  }

}
