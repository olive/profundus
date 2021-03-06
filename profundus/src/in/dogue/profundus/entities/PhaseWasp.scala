package in.dogue.profundus.entities

import in.dogue.antiqua.Antiqua._
import scala.util.Random
import in.dogue.antiqua.graphics.{TileRenderer, Animation, TileFactory}
import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.data.{Direction, CP437}
import in.dogue.profundus.lighting.LightSource
import in.dogue.profundus.world.{GlobalMessage, TerrainCache, WorldTile}
import in.dogue.profundus.particles.{RingParticle, DeathParticle, Particle}
import in.dogue.profundus.entities.damagezones.ExplosionZone
import in.dogue.profundus.Profundus
import in.dogue.profundus.deformations.ExplosionDeformation

object PhaseWasp {
  def create(ij:Cell, r:Random) = {
    val tf = TileFactory(Color.Black, Color.White)
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
    val wasp = PhaseWasp(anim)
    StandardEntity.create[PhaseWasp](_.update, _.draw, StandardEntity.NoMove, wasp, light, true, DamageType.PhaseWasp.some, 3, r).toEntity(ij)
  }
}

case class PhaseWasp(a:AnimationGroup) {
  final val moveTime = 30
  final val range = 30
  final val innerRange = 4
  final val attackTime = 60

  private def updateAnim = copy(a=a.smap {_.update})
  def update(health:Int, t:Int, args:EntityArgs): (PhaseWasp, Cell, Seq[GlobalMessage]) = {
    import Profundus._
    val pos = args.pos
    val pState = args.pi.live
    if (args.distance2 > range*range || pState == Dead) {
      return (updateAnim, pos, Seq())
    }
    val (newPos, canAttack) = if (t % moveTime == 0) {
      if (args.distance2 > innerRange*innerRange) {
        val newPos = args.toward.map {t => pos |+| t}.getOrElse(pos)
        (newPos, false)
      } else {
        (pos, true)
      }
    } else {
      (pos, false)
    }

    val gs = if (canAttack && t % attackTime == 0) {
      val ps = RingParticle.create(pos, 8, 3).toParticle
      val ex = ExplosionZone.create(pos, 8, 3, DamageType.PhaseWasp).toZone
      val df = ExplosionDeformation.create(pos, Damage(1, DamageType.PhaseWasp), 8, 3).toDeformation
      ps.seq.gms ++ ex.seq.gms ++ df.seq.gms
    } else {
      Seq()
    }

    (updateAnim, newPos, gs)
  }

  def draw(ij:Cell)(tr:TileRenderer):TileRenderer = {
    tr <++< a.map{ case (c, anim) => anim.drawFg(c |+| ij) _}
  }
}
