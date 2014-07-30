package in.dogue.profundus.world

import com.deweyvm.gleany.data.Recti
import scala.util.Random
import in.dogue.antiqua.data.Array2d
import in.dogue.antiqua.Antiqua
import Antiqua._

object Feature {
  def create(rect:Recti, f:(Int, Int, Int, TerrainScheme, Array2d[WorldTile], Random) => (Array2d[WorldTile], Seq[GlobalMessage])) = {
    Feature(rect, f, Seq())
  }
}

case class Feature private (rect:Recti, private val f:(Int, Int, Int, TerrainScheme, Array2d[WorldTile], Random) => (Array2d[WorldTile], Seq[GlobalMessage]), future:Seq[Feature]) {
  def withFuture(ft:Feature) = copy(future=future :+ ft)
  def intersects(other:Feature) = other.rect.intersects(rect)
  def transform(cols:Int,rows:Int,y:Int,ts:TerrainScheme, t:Array2d[WorldTile], r:Random) = {
    f(cols, rows, y, ts, t, r) @@ future
  }
}
