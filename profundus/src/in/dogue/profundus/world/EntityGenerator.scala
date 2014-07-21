package in.dogue.profundus.world

import in.dogue.profundus.entities._
import in.dogue.antiqua.data.Array2d
import scala.util.Random
import in.dogue.profundus.Profundus
import in.dogue.antiqua.Antiqua
import Antiqua._

object EntityGenerator {
  private def dummyFunc(cols:Int, rows:Int, i:Int, t:Array2d[WorldTile], r:Random) = {
    import Profundus._
    def isSolid(ij:Cell) = !t.get(ij).isWalkable
    val s = if (i <= 0) {
      Seq()
    } else {
      val creatures = (0 until 10).map { _ =>
        val pos = (r.nextInt(cols), r.nextInt(rows))
        Lurker.create(pos +| (i * rows)).onlyIf(!isSolid(pos))

      }.flatten
      val casques = (0 until 1).map { _ =>
        val pos = (r.nextInt(cols), r.nextInt(rows))

        Obelisk.create(pos +| (i * rows), r).onlyIf(!isSolid(pos))

      }.flatten
      val bats = (0 until 10).map { _ =>
        val pos = (r.nextInt(cols), r.nextInt(rows))
        Bat.create(pos +| (i * rows), r).onlyIf(!isSolid(pos))
      }.flatten

      val wasp = (0 until 10).map { _ =>
        val pos = (r.nextInt(cols), r.nextInt(rows))
        PhaseWasp.create(pos +| (i * rows), r).onlyIf(!isSolid(pos))
      }.flatten

      val bee = (0 until 10).map { _ =>
        val pos = (r.nextInt(cols), r.nextInt(rows))
        Bee.create(pos +| (i * rows), r).onlyIf(!isSolid(pos))
      }.flatten
      creatures ++ casques ++ bats ++ bee
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
