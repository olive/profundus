package in.dogue.profundus.entities

import in.dogue.antiqua.Antiqua._
import scala.util.Random
import in.dogue.antiqua.graphics.{TileRenderer, Animation, TileFactory}
import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.data.{Direction, CP437}
import in.dogue.profundus.lighting.LightSource
import in.dogue.profundus.world.{GlobalSpawn, TerrainCache, WorldTile}
import in.dogue.profundus.Profundus
import in.dogue.profundus.particles.{DeathParticle, Particle, RingParticle}
import in.dogue.profundus.entities.damagezones.{SingleTileZone, ExplosionZone}

object Bee {
  val attacking = Animation.create(Vector(
    (1, CP437.`@`.mkTile(Color.Black, Color.Red)),
    (1, CP437.ª.mkTile(Color.Black, Color.Yellow))

  ))
  val passive = Animation.singleton(CP437.ª.mkTile(Color.Black, Color.Yellow))
  def create(ij:Cell, r:Random) = {

    val light = LightSource.createCircle(ij, 0, 3, 0.2)
    val bee = Bee(passive, attacking, passive)
    StandardEntity.create[Bee](_.update, _.draw, bee, light, true, None, 1, r).toEntity(ij)
  }
}

case class Bee(a:Animation, b:Animation, drawAnim:Animation) {
  final val moveTime = 30
  final val range = 6
  final val innerRange = 4
  final val attackTime = 60

  def update(health:Int, t:Int, pos:Cell, cache:TerrainCache, ppos:Cell, pState:LivingState, r:Random): (Bee, Cell, Seq[GlobalSpawn]) = {
    import Profundus._

    val newSelf = copy(a = a.update, b = b.update)

    val diff = ppos |-| pos
    val isClose = diff.mag2 < range * range
    val hasLos = cache.hasLineOfSight(pos, ppos)
    val anim = if (isClose && hasLos) {
      b
    } else {
      a
    }
    val (newPos, kz) = if (isClose) {
      val isAdjacent = diff.x.abs + diff.y.abs == 1
      val inside = diff.x.abs + diff.y.abs == 0
      if (inside) {
        val newPos = if (t % 15 == 0) {
          val found = Direction.All.find(d => !cache.isSolid(pos -->d))
          found.map { d => pos --> d}.getOrElse(pos)
        } else {
          pos
        }
        (newPos, Seq())
      } else if (isAdjacent) {
        val kz = if (t %5 == 0) {
          Seq(SingleTileZone(pos |+| diff, 30, DamageType.Bee).toZone)
        } else {
          Seq()
        }
        (pos, kz)
      } else {
        val newPos = if (t % 15 == 0) {
          val tryPos = pos |+| diff.signum
          if (!cache.isSolid(tryPos)) {
            tryPos
          } else {
            pos
          }
        } else {
          pos
        }
        (newPos, Seq())
      }
    } else {
      if (t % 30 == 0) {
        val dir = Direction.All.randomR(r)
        val newPos = if (!cache.isSolid(pos --> dir)) {
          pos --> dir
        } else {
          pos
        }
        (newPos, Seq())
      } else {
        (pos, Seq())
      }


      //random walk
    }
    (newSelf.copy(drawAnim=anim), newPos, kz.gss)
  }


  def draw(ij:Cell)(tr:TileRenderer):TileRenderer = {
    tr <+< drawAnim.drawFg(ij)
  }


}
