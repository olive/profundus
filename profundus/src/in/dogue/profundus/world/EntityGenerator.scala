package in.dogue.profundus.world

import in.dogue.profundus.entities.{Bat, Casque, Lurker}
import in.dogue.antiqua.data.Array2d
import scala.util.Random
import in.dogue.profundus.Profundus
import in.dogue.antiqua.Antiqua
import Antiqua._

object EntityGenerator {
  private def dummyFunc(cols:Int, rows:Int, i:Int, t:Array2d[WorldTile], r:Random) = {
    import Profundus._
    def isSolid(ij:Cell) = !t.get(ij.x, ij.y).isWalkable
    val s = if (i <= 0) {
      Seq()
    } else {
      val creatures = (0 until 10).map { _ =>
        val pos = (r.nextInt(cols), r.nextInt(rows))
        if (!isSolid(pos)) {
          Lurker.create(pos +| (i * rows)).some
        } else {
          None
        }
      }.flatten
      val casques = (0 until 10).map { _ =>
        val pos = (r.nextInt(cols), r.nextInt(rows))
        if (!isSolid(pos)) {
          Casque.create(pos +| (i * rows), r).some
        } else {
          None
        }
      }.flatten
      val bats = (0 until 10).map { _ =>
        val pos = (r.nextInt(cols), r.nextInt(rows))
        if (!isSolid(pos)) {
          Bat.create(pos +| (i * rows), r).some
        } else {
          None
        }
      }.flatten
      creatures ++ casques ++ bats
    }
    s.ws
  }
  val dummy = EntityGenerator(dummyFunc)
}

case class EntityGenerator(f: (Int,Int,Int,Array2d[WorldTile], Random) => EntitySpawn) {
  def generate(cols:Int, rows:Int, i:Int, tiles:Array2d[WorldTile], r:Random):EntitySpawn = {
    f(cols, rows, i, tiles, r)
  }
}
