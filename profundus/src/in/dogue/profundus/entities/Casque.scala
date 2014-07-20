package in.dogue.profundus.entities

import in.dogue.antiqua.Antiqua.{TileGroup, Cell}
import in.dogue.antiqua.data.{Direction, CP437}
import in.dogue.antiqua.graphics.{TileRenderer, Tile}
import com.deweyvm.gleany.graphics.Color
import scala.util.Random
import in.dogue.profundus.entities.damagezones.ExplosionZone
import in.dogue.profundus.world._
import in.dogue.profundus.Profundus
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.profundus.particles.{RingParticle, DeathParticle, Particle, ExplosionParticle}
import in.dogue.profundus.entities.pickups.ItemPickup
import in.dogue.profundus.particles.Particle
import in.dogue.profundus.world.WorldTile
import in.dogue.profundus.entities.pickups.ItemPickup
import in.dogue.profundus.lighting.LightSource
import in.dogue.profundus.audio.SoundManager

object Casque {
  val attackTime = 10*60
  def create(ij:Cell, r:Random) = {
    val tg = Tile.makeGroup(Vector(
      (0, 0, CP437.ç.toCode, Color.Black, Color.White),
      (0, 1, CP437.↑.toCode, Color.Black, Color.White)
    ))
    val light = LightSource.createCircle(ij, 3, 4, 0.5)
    Casque(tg, 50, math.abs(r.nextInt()), light, Alive).toEntity(ij)

  }
}
case class Casque private (tg:TileGroup, health:Int, t:Int, light:LightSource, live:LivingState) {
  import Profundus._

  def damage(dmg:Damage) = {
    if (dmg.source == DamageType.Obelisk) {
      this
    } else {
      copy(health=health.drop(dmg.amount))
    }

  }
  def getLive = live
  def move(ij:Cell, from:Direction, newTouching:Direction => Option[WorldTile]): Casque = {
    this
  }

  def kill = copy(live=Dead)

  def update(pos:Cell, cache:TerrainCache, ppos:Cell, pState:LivingState, r:Random):(Casque, Cell, Seq[GlobalSpawn], Seq[WorldSpawn]) = {
    val spawns = if (t > 0 && t % Casque.attackTime == 0) {
      SoundManager.pop.play()
      val ps = RingParticle.create(pos, 8, 3).toParticle
      val ex = ExplosionZone.create(pos, 8, 3, DamageType.Obelisk).toZone
      Seq(Seq(ps).gs, Seq(ex).gs)
    } else {
      Seq()
    }
    val (newLive, pickups) = if (health <= 0){
      (Dead, Seq(ItemPickup(pos, Trampoline).toPickup))
    } else {
      (live, Seq())
    }
    (copy(t=t+1, live=newLive), pos, spawns, Seq(pickups.ws))
  }

  def getDeathParticle(ij:Cell):Particle[_] = DeathParticle.create(ij, 60).toParticle

  def draw(ij:Cell)(tr:TileRenderer):TileRenderer = {
    tr <|| (tg |+| (ij.x, ij.y))
  }

  def getLight(ij:Cell) = Seq(light.copy(pos=ij))

  def toEntity(ij:Cell):Entity[Casque] = {
    Entity(ij, Floating, _.update, _.move, _.damage, _.kill, _.getDeathParticle, _.getLight, _.getLive, _.draw, this)
  }
}
