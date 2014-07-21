package in.dogue.profundus.entities

import in.dogue.antiqua.Antiqua._
import scala.util.Random
import in.dogue.antiqua.graphics.{TileRenderer, Animation, TileFactory}
import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.data.{Direction, CP437}
import in.dogue.profundus.lighting.LightSource
import in.dogue.profundus.world.{WorldSpawn, GlobalSpawn, TerrainCache, WorldTile}
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
    Bee(passive, attacking, passive, light, Alive, 1, math.abs(r.nextInt)).toEntity(ij)
  }
}

case class Bee(a:Animation, b:Animation, drawAnim:Animation, light:LightSource, live:LivingState, health:Int, t:Int) {
  final val moveTime = 30
  final val range = 6
  final val innerRange = 4
  final val attackTime = 60
  def damage(dmg:Damage) =  copy(health=health.drop(dmg.amount))
  def getLive = live
  def move(ij:Cell, from:Direction, newTouching:Direction => Option[WorldTile]) = {
    this
  }

  def kill = copy(live=Dead)

  def update(pos:Cell, cache:TerrainCache, ppos:Cell, pState:LivingState, r:Random): (Bee, Cell, Seq[GlobalSpawn], Seq[WorldSpawn]) = {
    import Profundus._

    val killed = if (health <= 0) {
      kill
    } else {
      this
    }
    val newSelf = killed.copy(t = t + 1, a = a.update, b = b.update)
    if (pState == Dead) {
      return (newSelf, pos, Seq(), Seq())
    }
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
      if (isAdjacent) {
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
    (newSelf.copy(drawAnim=anim), newPos, Seq(kz.gs), Seq())
  }

  def getDeathParticle(ij:Cell):Particle[_] = DeathParticle.create(ij, 60).toParticle

  def draw(ij:Cell)(tr:TileRenderer):TileRenderer = {
    tr <+< drawAnim.drawFg(ij)
  }

  def getLight(ij:Cell) = Seq(light.copy(pos=ij))

  def toEntity(ij:Cell):Entity[Bee] = {
    Entity(ij, Floating, _.update, _.move, _.damage, _.kill, _.getDeathParticle, _.getLight, _.getLive, _.draw, this)
  }

}
