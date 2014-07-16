package in.dogue.profundus

import in.dogue.antiqua.graphics.{Border, TextFactory, Tile}
import in.dogue.profundus.particles.Particle
import in.dogue.profundus.experimental.{NewDeformations, NewKillZones, NewParticles}
import in.dogue.profundus.entities.KillZone
import in.dogue.profundus.deformations.Deformation
import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.data.CP437
import sun.io.CharToByteCp437

object Profundus {
  class AugNewParticles(s:Seq[Particle[_]]) {
    def ns = NewParticles(s)
  }
  implicit def particles2NewSpawn(s:Seq[Particle[_]]) = new AugNewParticles(s)

  class AugNewKillZones(s:Seq[KillZone[_]]) {
    def ns = NewKillZones(s)
  }
  implicit def killZones2NewSpawn(s:Seq[KillZone[_]]) = new AugNewKillZones(s)
  class AugNewDeformations(s:Seq[Deformation[_]]) {
    def ns = NewDeformations(s)
  }
  implicit def deformations2NewSpawn(s:Seq[Deformation[_]]) = new AugNewDeformations(s)

  val tf = TextFactory(Color.Black, Color.White, CP437.unicodeToCode)
  val border = Border(CP437.doubleBorder)(Color.Black, Color.White) _
}
