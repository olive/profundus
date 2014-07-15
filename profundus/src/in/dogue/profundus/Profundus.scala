package in.dogue.profundus

import in.dogue.antiqua.graphics.Tile
import in.dogue.profundus.particles.Particle
import in.dogue.profundus.experimental.{NewDeformations, NewKillZones, NewParticles}
import in.dogue.profundus.entities.KillZone
import in.dogue.profundus.deformations.Deformation

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
}
