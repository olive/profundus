package in.dogue.profundus.world

import scala.util.Random
import in.dogue.profundus.entities.pickups._
import in.dogue.profundus.Profundus
import in.dogue.antiqua.data.Array2d
import in.dogue.antiqua.Antiqua
import Antiqua._

object PickupGenerator {

  val dummy = {
    def gen(ff:FoodFactory, cols:Int, rows:Int, yRoom:Int, cache:Array2d[WorldTile], ts:TerrainScheme, r:Random) = {

      val foods = ff.All.map { ft =>
        (0 until 10).map { i =>
          val x = r.nextInt(cols)
          val y = r.nextInt(rows)
          FoodPickup.create((x, y + yRoom), ft, r).toPickup.onlyIf(cache.get((x, y)).isWalkable)
        }.flatten
      }
      ts.color.ways1(foods)

    }
    PickupGenerator(gen)
  }

  val empty = {
    def gen(ff:FoodFactory, cols:Int, rows:Int, yRoom:Int, cache:Array2d[WorldTile], ts:TerrainScheme, r:Random) = Seq()
    PickupGenerator(gen)
  }
}

case class PickupGenerator(private val f:(FoodFactory, Int,Int,Int,Array2d[WorldTile], TerrainScheme, Random) => Seq[Pickup]) {
  def generate(ff:FoodFactory, cols:Int, rows:Int, y:Int, cache:Array2d[WorldTile], ts:TerrainScheme, r:Random):Seq[GlobalMessage] = {
    import Profundus._
    val picks = f(ff, cols, rows, y, cache, ts, r)
    picks.gms
  }
}
