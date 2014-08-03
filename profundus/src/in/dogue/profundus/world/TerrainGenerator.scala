package in.dogue.profundus.world

import scala.util.Random
import in.dogue.antiqua.Antiqua
import Antiqua._

object TerrainGenerator {
  def mkEmpty(ts:TerrainScheme, tf:WorldTileFactory, ij:Cell, y:Int, cols:Int, rows:Int, d:Double, r:Random):(WorldTile, Option[Cell]) = {
    tf.mkEmpty
  }

  def mkUnloaded(ts:TerrainScheme, tf:WorldTileFactory, ij:Cell, y:Int, cols:Int, rows:Int, d:Double, r:Random): (WorldTile, Option[Cell]) = {
    tf.mkShaft
  }

  def test(ts:TerrainScheme, tf:WorldTileFactory, ij:Cell, y:Int, cols:Int, rows:Int, d:Double, r:Random): (WorldTile, Option[Cell]) = {
    val vs:Vector[Double] = (0 until 8).map { (i:Int) => 0.2 * (i/8.toDouble) - 0.2 }.toVector
    val base = ts.color.ways1(vs)
    val incr = 0.2
    if (ij.x < 2 || ij.x > cols - 3) {
      tf.mkShaft
    } else if (d.inRange(base, 1)) {
      tf.mkEmpty
    } else if (d.inRange(base - incr, base)) {
      tf.mkDirt
    } else if (d.inRange(base - incr*2, base - incr)) {
      tf.mkClay
    } else if (d.inRange(base - incr*3, base - incr*2)) {
      tf.mkRock1
    } else {
      tf.mkRock2
    }
  }

  def dummy = TerrainGenerator(test)
  def empty = TerrainGenerator(mkEmpty)
  def unloaded = TerrainGenerator(mkUnloaded)
  type Generate = (TerrainScheme, WorldTileFactory, Cell,Int,Int,Int,Double,Random) => (WorldTile, Option[Cell])
}
case class TerrainGenerator(private val mkTile:TerrainGenerator.Generate) {
  def generate(ts:TerrainScheme, tf:WorldTileFactory, ij:Cell, y:Int, cols:Int, rows:Int, d:Double, r:Random) =
    mkTile(ts, tf, ij, y, cols, rows, d, r)
}
