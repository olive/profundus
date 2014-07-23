package in.dogue.profundus.world

import in.dogue.profundus.entities._
import in.dogue.antiqua.data.Array2d
import scala.util.Random
import in.dogue.profundus.Profundus
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.antiqua.data.Direction.Down
import scala.collection.immutable.IndexedSeq
import in.dogue.profundus.utils.MessageBoxReader

object EntityGenerator {
  private def dummyFunc(cols:Int, rows:Int, i:Int, ts:TerrainScheme, t:Array2d[WorldTile], r:Random) = {
    import Profundus._
    def isSolid(ij:Cell) = !t.getOption(ij).exists{_.isWalkable}
    val s:Seq[Entity[_]] = if (i <= 0) {
      Seq()
    } else {
      val creatures:Seq[Entity[_]] = (0 until 10).map { _ =>
        val pos = (r.nextInt(cols), r.nextInt(rows))
        Lurker.create(pos +| (i * rows)).onlyIf(!isSolid(pos))

      }.flatten
      val casques:Seq[Entity[_]] = (0 until 1).map { _ =>
        val pos = (r.nextInt(cols), r.nextInt(rows))

        Obelisk.create(pos +| (i * rows), r).onlyIf(!isSolid(pos)
                                                 && !isSolid(pos --> Down)
                                                 && isSolid(pos --> Down --> Down))

      }.flatten
      val bats:Seq[Entity[_]] = (0 until 10).map { _ =>
        val pos = (r.nextInt(cols), r.nextInt(rows))
        Bat.create(pos +| (i * rows), r).onlyIf(!isSolid(pos))
      }.flatten

      val wasp:Seq[Entity[_]] = (0 until 1).map { _ =>
        val pos = (r.nextInt(cols), r.nextInt(rows))
        PhaseWasp.create(pos +| (i * rows), r).onlyIf(!isSolid(pos))
      }.flatten

      val bee:Seq[Entity[_]] = (0 until 10).map { _ =>
        val pos = (r.nextInt(cols), r.nextInt(rows))
        Bee.create(pos +| (i * rows), r).onlyIf(!isSolid(pos))
      }.flatten
      val all = Vector(creatures, casques, bats, bee, wasp)
      val (a, b) = ts.color.ways2(all)
      a ++ b
    }
    s.ws
  }

  private def lairFunc(cols:Int, rows:Int, i:Int, ts:TerrainScheme, t:Array2d[WorldTile], r:Random) = {
    val pos =  (cols - 14, rows - 2) +| (i * rows)
    val tbs = (1 to 4) map { case i =>
      val name = "fakeending" + i
      MessageBoxReader.load(name)
    }
    EntitySpawn(Seq(Phoebe.create(pos, tbs.toVector)))
  }

  private def emptyFunc(cols:Int, rows:Int, i:Int, ts:TerrainScheme, t:Array2d[WorldTile], r:Random) = {
    EntitySpawn(Seq())
  }



  val dummy = EntityGenerator(dummyFunc)
  val lair = EntityGenerator(lairFunc)
  val empty = EntityGenerator(emptyFunc)
}

case class EntityGenerator(f: (Int,Int,Int,TerrainScheme, Array2d[WorldTile], Random) => EntitySpawn) {
  def generate(cols:Int, rows:Int, i:Int, ts:TerrainScheme,tiles:Array2d[WorldTile], r:Random):EntitySpawn = {
    f(cols, rows, i, ts, tiles, r)
  }
}
