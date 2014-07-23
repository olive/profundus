package in.dogue.profundus.world

import in.dogue.antiqua.data.Direction
import scala.util.Random
import in.dogue.antiqua.Antiqua.Cell
import in.dogue.antiqua.geometry.Circle

object SpawnGenerator {
  def dummy = SpawnGenerator[Unit]{ case _ => ((0,0), Direction.Down, ())}
  def surface = SpawnGenerator[(Direction, Vector[Seq[Cell]], Circle)](caveFace)
  private def caveFace(cols:Int, rows:Int, r:Random) = {
    Terrain.makeLines(cols, rows, r)
  }

}
case class SpawnGenerator[T](gen:(Int,Int,Random) => (Cell, Direction, T)) {

}
