package in.dogue.profundus.world.features

import in.dogue.antiqua.Antiqua.Cell
import in.dogue.profundus.world.{Terrain, Feature, WorldTile, TerrainScheme}
import in.dogue.antiqua.data.Array2d
import scala.util.Random
import in.dogue.profundus.doodads.Campfire
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.antiqua.geometry.{Line, Circle}
import com.deweyvm.gleany.data.Recti
import in.dogue.profundus.Profundus

case class Campsite(center:Cell, radius:Int) {
  def placeSite(cols:Int, rows:Int, yy:Int, scheme:TerrainScheme, terrain:Array2d[WorldTile], r:Random) = {
    import Profundus._
    val tf = scheme.toFactory(r)
    val fillDepth = center +| (radius/2)
    val circle = Circle(center, radius)
    val line = Line.bresenham(center.x, center.y, center.x + radius, center.y)
    val line2 = Line.bresenham(center.x, center.y, center.x - radius, center.y)
    val (tiles, deps) = terrain.map { case (p, t) =>
      val contains = circle.contains(p)
      if (line.contains(p) || line2.contains(p)) {
        tf.mkEmpty
      } else if (contains && p.y > fillDepth.y) {
        tf.mkDirt
      } else if (contains && (center |-| p).mag > 3*radius/4){
        tf.mkDirt
      } else if (contains) {
        tf.mkEmpty
      } else {
        t @@ None
      }
    }.unzip
    val newTiles = Terrain.merge(tiles, deps)
    val siteSpot = center +| yy +| radius/2
    (newTiles, Seq(Campfire.create(siteSpot).toDoodad).gss)
  }
  def toFeature(cols:Int, rows:Int):Feature = {
    val rect = Recti(center.x - radius/2, center.y - radius/2, radius*2, radius*2)
    val f = placeSite _
    Feature(rect, f)
  }

}
