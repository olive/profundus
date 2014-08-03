package in.dogue.profundus.world.features

import in.dogue.profundus.world._
import in.dogue.antiqua.data.Array2d
import scala.util.Random
import in.dogue.antiqua.geometry.Circle
import in.dogue.profundus.doodads.Campfire
import com.deweyvm.gleany.data.Recti
import in.dogue.antiqua.Antiqua._
import in.dogue.profundus.world.WorldTile
import in.dogue.profundus.world.Feature

case class Cavern(center:Cell, radius:Int) {
  def placeSite(cols:Int, rows:Int, yy:Int, scheme:TerrainScheme, terrain:Array2d[WorldTile], r:Random) = {
    val tf = scheme.toFactory(r)
    val fillDepth = center -| (3*radius/4)
    val circle = Circle(center, radius)
    val (nt, gen) = terrain.map { case (p, t) =>
      val contains = circle.contains(p)
      if (contains && (center |-| p).mag > radius) {
        tf.mkDirt
      } else if (contains && p.y > fillDepth.y) {
        tf.mkEmpty
      } else if (contains){
        tf.mkEmpty
      } else {
        t @@ None
      }

    }.unzip
    val newTiles = Terrain.merge(nt, gen)
    newTiles @@ Seq()
  }
  def toFeature(yPos:Int, cols:Int, rows:Int):Feature = {
    val rect = Recti(center.x - radius/2, center.y - radius/2 + yPos, radius*2, radius*2)
    val f = placeSite _
    Feature.create(false, rect, f)
  }
}
