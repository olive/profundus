package in.dogue.profundus.world.features

import in.dogue.profundus.world.{Feature, WorldTile, TerrainScheme}
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
    val fillDepth = center -| (3*radius/4)
    val circle = Circle(center, radius)
    val newTiles = terrain.map { case (p, t) =>
      val contains = circle.contains(p)
      if (contains && (center |-| p).mag > radius) {
        WorldTile(scheme.makeDirt(r))
      } else if (contains && p.y > fillDepth.y) {
        WorldTile(scheme.makeEmpty(r))
        //WorldTile(scheme.makeDirt(r))
      } else if (contains){
        WorldTile(scheme.makeEmpty(r))
      } else {
        t
      }

    }
    newTiles @@ Seq()
  }
  def toFeature(cols:Int, rows:Int):Feature = {
    val rect = Recti(center.x - radius/2, center.y - radius/2, radius*2, radius*2)
    val f = placeSite _
    Feature(rect, f)
  }
}
