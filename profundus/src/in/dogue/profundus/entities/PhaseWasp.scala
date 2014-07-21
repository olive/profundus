package in.dogue.profundus.entities

import in.dogue.antiqua.Antiqua._
import scala.util.Random
import in.dogue.antiqua.graphics.{TileRenderer, Animation, TileFactory}
import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.data.{Direction, CP437}
import in.dogue.profundus.lighting.LightSource
import in.dogue.profundus.world.{WorldSpawn, GlobalSpawn, TerrainCache, WorldTile}
import in.dogue.profundus.particles.{RingParticle, DeathParticle, Particle}
import in.dogue.profundus.entities.damagezones.ExplosionZone
import in.dogue.profundus.Profundus
import in.dogue.profundus.deformations.ExplosionDeformation

object PhaseWasp {
  def create(ij:Cell, r:Random) = {
    val tf = TileFactory(Color.Black, Color.Black)
    val rightWing = Animation.create(Vector(
      (15, tf(CP437.<)),
      (15, tf(CP437.`«`))

    ))
    val leftWing = Animation.create(Vector(
      (15, tf(CP437.>)),
      (15, tf(CP437.`»`))

    ))
    val body = Animation.singleton(tf(CP437.!))
    val anim = Vector(
      ((-1,0), leftWing),
      ((0,0), body),
      ((1,0), rightWing)
    )
    val light = LightSource.createCircle(ij, 0, 3, 0.2)
    PhaseWasp(anim, light, Alive, 12, math.abs(r.nextInt)).toEntity(ij)
  }
}

case class PhaseWasp(a:AnimationGroup, light:LightSource, live:LivingState, health:Int, t:Int) {
  final val moveTime = 30
  final val range = 30
  final val innerRange = 4
  final val attackTime = 60
  def damage(dmg:Damage) = {
    if (dmg.source == DamageType.PhaseWasp) {
      this
    } else {
      copy(health=health.drop(dmg.amount))
    }
  }
  def getLive = live
  def move(ij:Cell, from:Direction, newTouching:Direction => Option[WorldTile]) = {
    this
  }

  def kill = copy(live=Dead)
  private def updateAnim = copy(a=a.smap {_.update})
  def update(pos:Cell, cache:TerrainCache, ppos:Cell, pState:LivingState, r:Random): (PhaseWasp, Cell, Seq[GlobalSpawn], Seq[WorldSpawn]) = {
    import Profundus._
    val diff = ppos |-| pos
    if (diff.mag2 > range*range || pState == Dead) {
      return (copy(t=t+1).updateAnim, pos, Seq(), Seq())
    }
    val d = diff.signum
    val (newPos, canAttack) = if (t % moveTime == 0) {
      if (diff.mag2 > innerRange*innerRange) {
        (pos |+| d, false)
      } else {
        (pos, true)
      }
    } else {
      (pos, false)
    }

    val gs = if (canAttack && t % attackTime == 0) {
      val ps = RingParticle.create(pos, 8, 3).toParticle
      val ex = ExplosionZone.create(pos, 8, 3, DamageType.PhaseWasp).toZone
      val df = ExplosionDeformation.create(pos, 5, 8, 3).toDeformation
      Seq(Seq(ps).gs, Seq(ex).gs, Seq(df).gs)
    } else {
      Seq()
    }

    val killed = if (health <= 0) {
      this.kill
    } else {
      this
    }

    (killed.copy(t=t+1).updateAnim, newPos, gs, Seq())
  }

  def getDeathParticle(ij:Cell):Particle[_] = DeathParticle.create(ij, 60).toParticle

  def draw(ij:Cell)(tr:TileRenderer):TileRenderer = {
    tr <++< a.map{ case (c, anim) => anim.drawFg(c |+| ij) _}
  }

  def getLight(ij:Cell) = Seq(light.copy(pos=ij))

  def toEntity(ij:Cell):Entity[PhaseWasp] = {
    Entity(ij, Floating, _.update, _.move, _.damage, _.kill, _.getDeathParticle, _.getLight, _.getLive, _.draw, this)
  }
}
