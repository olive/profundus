package in.dogue.profundus.world

import scala.util.Random
import in.dogue.antiqua.Antiqua.Cell
import in.dogue.antiqua.Antiqua
import Antiqua._

object TerrainGenerator {
  def upper(scheme:TerrainScheme, ij:Cell, y:Int, cols:Int, rows:Int, d:Double, r:Random) = {
    val i = ij.x
    val j = ij.y
    val func = if (i <= 0 || (i <= 1 && r.nextDouble < 0.6) || i >= cols - 1 || (i >= cols - 2 && r.nextDouble < 0.6)) {
      scheme.makeShaft _
    } else if (d < -0.2) {
      if (r.nextDouble > 0.99) {
        if (r.nextBoolean) {
          scheme.makeRock3 _
        } else {
          scheme.makeMineral _
        }
      } else if (d < -0.6) {
        scheme.makeRock2 _
      } else if (d < -0.4){
        scheme.makeRock _
      } else {
        scheme.makeClay _
      }
    } else if (d < 0.0) {
      scheme.makeDirt _
    } else {
      scheme.makeEmpty _
    }
    func
  }
  def dummy(ts:TerrainScheme) = TerrainGenerator(upper)
  type Generate = (TerrainScheme, Cell,Int,Int,Int,Double,Random) => (Random => TileType)
}
case class TerrainGenerator(mkTile:TerrainGenerator.Generate) {
  def generate(ts:TerrainScheme, ij:Cell, y:Int, cols:Int, rows:Int, d:Double, r:Random) =
    mkTile(ts, ij, y, cols, rows, d, r)
}
