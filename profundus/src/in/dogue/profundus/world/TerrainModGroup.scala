package in.dogue.profundus.world

import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.antiqua.data.Array2d
import scala.util.Random
import in.dogue.profundus.Profundus

object TerrainModGroup {
  def spikes(num:Int) = TerrainModGroup(TerrainMod.spikes(num).seq)
  val empty = TerrainModGroup(Seq())
}

case class TerrainModGroup(mods:Seq[TerrainMod]) {
  def mod(tf:WorldTileFactory, y:Int, arr:Array2d[WorldTile], r:Random) = {
    Profundus.fold2(arr, mods) { case (mod, terrain) =>
      mod.mod(tf, y, terrain, r)
    }
  }
}
