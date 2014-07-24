package in.dogue.profundus.world

import scala.util.Random
import in.dogue.antiqua.Antiqua.Cell
import in.dogue.antiqua.Antiqua
import Antiqua._

object TerrainGenerator {
  def mkEmpty(ts:TerrainScheme, ij:Cell, y:Int, cols:Int, rows:Int, d:Double, r:Random) = {
    ts.makeEmpty _
  }
  def test(ts:TerrainScheme, ij:Cell, y:Int, cols:Int, rows:Int, d:Double, r:Random) = {
    val vs:Vector[Double] = (0 until 8).map { (i:Int) => 0.2 * (i/8.toDouble) - 0.2 }.toVector
    val base = ts.color.ways1(vs)
    val incr = 0.2
    if (ij.x < 2 || ij.x > cols - 3) {
      ts.makeShaft _
    } else if (d.inRange(base, 1)) {
      ts.makeEmpty _
    } else if (d.inRange(base - incr, base)) {
      ts.makeDirt _
    } else if (d.inRange(base - incr*2, base - incr)) {
      ts.makeClay _
    } else if (d.inRange(base - incr*3, base - incr*2)) {
      ts.makeRock _
    } else {
      ts.makeRock2 _
    }

  }

  def dummy(ts:TerrainScheme) = TerrainGenerator(test)
  def empty(ts:TerrainScheme) = TerrainGenerator(mkEmpty)
  type Generate = (TerrainScheme, Cell,Int,Int,Int,Double,Random) => (Random => TileType)
}
case class TerrainGenerator(mkTile:TerrainGenerator.Generate) {
  def generate(ts:TerrainScheme, ij:Cell, y:Int, cols:Int, rows:Int, d:Double, r:Random) =
    mkTile(ts, ij, y, cols, rows, d, r)
}
