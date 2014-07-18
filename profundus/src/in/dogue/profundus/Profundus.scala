package in.dogue.profundus

import in.dogue.antiqua.graphics.{Border, TextFactory, Tile}
import in.dogue.profundus.particles.Particle
import in.dogue.profundus.deformations.Deformation
import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.data.CP437
import sun.io.CharToByteCp437
import in.dogue.profundus.world.{NewDeformations, NewDamageZones, NewParticles}
import in.dogue.profundus.entities.damagezones.DamageZone

object Profundus {
  class AugNewParticles(s:Seq[Particle[_]]) {
    def ns = NewParticles(s)
  }
  implicit def particles2NewSpawn(s:Seq[Particle[_]]) = new AugNewParticles(s)

  class AugNewDamageZones(s:Seq[DamageZone[_]]) {
    def ns = NewDamageZones(s)
  }
  implicit def damageZones2NewSpawn(s:Seq[DamageZone[_]]) = new AugNewDamageZones(s)
  class AugNewDeformations(s:Seq[Deformation[_]]) {
    def ns = NewDeformations(s)
  }
  implicit def deformations2NewSpawn(s:Seq[Deformation[_]]) = new AugNewDeformations(s)

  val tf = TextFactory(Color.Black, Color.White, CP437.unicodeToCode)
  val border = Border(CP437.doubleBorder)(Color.Black, Color.White) _
}
