package in.dogue.profundus.world

import scala.util.Random

object TerrainGenerator {
  type Generate = (Int,Int,Int,Int,Int,Double,Random) => (Random => TileType)
}
case class TerrainGenerator(scheme:TerrainScheme, mkTile:TerrainGenerator.Generate) {
  def generate(i:Int, j:Int, y:Int, cols:Int, rows:Int, d:Double, r:Random) =
    mkTile(i, j, y, cols, rows, d, r)
}
