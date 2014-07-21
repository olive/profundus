package in.dogue.profundus.world

import scala.util.Random
import in.dogue.profundus.entities.pickups.{FoodPickup, FoodType, Pickup}
import in.dogue.profundus.Profundus
import in.dogue.antiqua.data.Array2d

object PickupGenerator {
  val dummy = {
    def gen(cols:Int, rows:Int, cache:Array2d[WorldTile], ts:TerrainScheme, r:Random) = {
      (0 until 10) map { i =>
        val x = r.nextInt(cols)
        val y = r.nextInt(rows)
        val ftype = FoodType.random(r)
        FoodPickup.create((x, y), ftype).toPickup
      }

    }
    PickupGenerator(gen)
  }
}

case class PickupGenerator(private val f:(Int,Int,Array2d[WorldTile], TerrainScheme, Random) => Seq[Pickup[_]]) {
  def generate(cols:Int, rows:Int, cache:Array2d[WorldTile], ts:TerrainScheme, r:Random):Seq[WorldSpawn] = {
    import Profundus._
    val picks = f(cols, rows, cache, ts, r)
    Seq(picks.ws)
  }
}
