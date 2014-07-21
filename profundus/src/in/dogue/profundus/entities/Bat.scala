package in.dogue.profundus.entities

import in.dogue.antiqua.Antiqua._
import in.dogue.antiqua.data.{CP437, Direction}
import in.dogue.profundus.world.{WorldSpawn, GlobalSpawn, TerrainCache, WorldTile}
import scala.util.Random
import in.dogue.profundus.particles.{DeathParticle, Particle, RingParticle}
import in.dogue.profundus.entities.damagezones.{SingleTileZone, ExplosionZone}
import in.dogue.profundus.entities.pickups.ItemPickup
import in.dogue.antiqua.graphics.{Tile, TileFactory, Animation, TileRenderer}
import com.deweyvm.gleany.graphics.Color
import in.dogue.profundus.lighting.LightSource
import in.dogue.profundus.Profundus

object Bat {
  def create(ij:Cell, r:Random) = {
    val tf = TileFactory(Color.Black, Color.Red.dim(2))
    val rightWing = Animation.create(Vector(
      (15, tf(CP437.¬)),
      (15, tf(CP437.^))

    ))
    val leftWing = Animation.create(Vector(
      (15, tf(CP437.⌐)),
      (15, tf(CP437.^))

    ))
    val body = Animation.singleton(tf(CP437.◦))
    val anim = Vector(
      ((-1,0), leftWing),
      ((0,0), body),
      ((1,0), rightWing)
    )
    val light = LightSource.createCircle(ij, 0, 3, 0.2)
    Bat(anim, light, Alive, 12, math.abs(r.nextInt)).toEntity(ij)
  }
}

case class Bat(a:AnimationGroup, light:LightSource, live:LivingState, health:Int, t:Int) {
  def damage(dmg:Damage) = copy(health=health.drop(dmg.amount))
  def getLive = live
  def move(ij:Cell, from:Direction, newTouching:Direction => Option[WorldTile]): Bat = {
    this
  }

  def kill = copy(live=Dead)

  def update(pos:Cell, cache:TerrainCache, ppos:Cell, pState:LivingState, r:Random):(Bat, Cell, Seq[GlobalSpawn], Seq[WorldSpawn]) = {
    import Profundus._
    val dd = ppos |-| pos
    val isAdjacent = math.abs(dd.x) + math.abs(dd.y) == 1
    val newPos = if (dd == ((0,0))) {
      val move = Direction.All.find { d =>
        !cache.isSolid(pos --> d)
      }
      move.map{pos --> _}.getOrElse(pos)
    } else if (!isAdjacent && t % 5 == 0 && cache.hasLineOfSight(pos, ppos)) {
      val move =  dd.signum
      val res = pos |+| move
      if (!cache.isSolid(res)) {
        res
      } else {
        pos
      }

    } else if (t % 15 == 0) {
      val d = Direction.All.randomR(r)
      if (!cache.isSolid(pos --> d)) {
        pos --> d
      } else {
        pos
      }
    } else {
      pos
    }
    val attack = if (isAdjacent && t % 15 == 0) {
      Seq(SingleTileZone(ppos, 75, DamageType.HellBat).toZone)
    } else {
      Seq()
    }

    val updated = copy(a=a.smap{_.update}, t=t+1)
    val killed = if (health <= 0) {
      updated.kill
    } else {
      updated
    }

    (killed, newPos, Seq(attack.gs), Seq())
  }

  def getDeathParticle(ij:Cell):Particle[_] = DeathParticle.create(ij, 60).toParticle

  def draw(ij:Cell)(tr:TileRenderer):TileRenderer = {
    tr <++< a.map{ case (c, anim) => anim.drawFg(c |+| ij) _}
  }

  def getLight(ij:Cell) = Seq(light.copy(pos=ij))

  def toEntity(ij:Cell):Entity[Bat] = {
    Entity(ij, Floating, _.update, _.move, _.damage, _.kill, _.getDeathParticle, _.getLight, _.getLive, _.draw, this)
  }
}
