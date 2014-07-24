package in.dogue.profundus.world

import in.dogue.antiqua.Antiqua._

case class NewTerrainCache(cols:Int, rows:Int, tMap:Map[Int,(Stratum, Terrain)]) {
  def update(ppos:Cell):(NewTerrainCache, Seq[GlobalSpawn], Seq[WorldSpawn]) = {
    val newI = getIndex(ppos)
    this @@ Seq() @@ Seq()
  }

  private def getIndex(ij:Cell) = {
    val y = ij.y
    val yy = (y < 0).select(y, y-(rows-1))
    yy/rows
  }


  private def convert(ij:Cell):Cell = {
    (ij.x, ij.y %% rows)
  }
}
