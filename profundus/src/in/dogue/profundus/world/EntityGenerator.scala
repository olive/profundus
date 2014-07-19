package in.dogue.profundus.world

import in.dogue.profundus.entities.{Casque, Creature}
import in.dogue.antiqua.data.Array2d
import scala.util.Random

object EntityGenerator {
  private def dummyFunc(cols:Int, rows:Int, i:Int, t:Array2d[WorldTile], r:Random) = {
    if (i <= 0) {
      (Seq(), Seq())
    } else {
      val creatures = (0 until 10) map {
        case _ =>
          val (x, y) = (r.nextInt(cols), r.nextInt(rows))
          Creature.create(x, y + i * rows)
      }
      val casques = (0 until 10) map {
        case _ =>
          val (x, y) = (r.nextInt(cols), r.nextInt(rows))
          Casque.create((x, y + i * rows), r)

      }
      (creatures, casques)
    }
  }
  val dummy = EntityGenerator(dummyFunc)
}

case class EntityGenerator(f: (Int,Int,Int,Array2d[WorldTile], Random) => (Seq[Creature], Seq[Casque])) {
  def generate(cols:Int, rows:Int, i:Int, tiles:Array2d[WorldTile], r:Random):(Seq[Creature], Seq[Casque]) = {
    f(cols, rows, i, tiles, r)
  }
}
