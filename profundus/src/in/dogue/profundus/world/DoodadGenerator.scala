package in.dogue.profundus.world

import in.dogue.profundus.doodads.Doodad
import in.dogue.antiqua.data.Array2d
import scala.util.Random

object DoodadGenerator {
  private def dummyFunc(ts:TerrainScheme, tiles:Array2d[WorldTile], r:Random) = Seq()
  val empty = DoodadGenerator(dummyFunc)
}

case class DoodadGenerator(f: (TerrainScheme, Array2d[WorldTile], Random) => Seq[Doodad]) {
  def generate(ts:TerrainScheme, tiles:Array2d[WorldTile], r:Random) = f(ts,tiles,r)
}
