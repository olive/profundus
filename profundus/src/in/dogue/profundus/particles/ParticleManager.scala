package in.dogue.profundus.particles

import in.dogue.antiqua.graphics.TileRenderer
import in.dogue.profundus.lighting.LightSource
import in.dogue.profundus.world.TerrainCache


object ParticleManager {
  def create = {
    ParticleManager(Seq(), Seq())
  }
}

case class ParticleManager private (emitters:Seq[Emitter], ps:Seq[Particle]) {
  def update(tc:TerrainCache) = {
    val (ems, emps) = emitters.map {e =>
      e.update : (Option[(Emitter, Seq[Particle])])
    }.flatten.unzip
    val (updated, lights) = ps.map{_.update(tc)}.flatten.unzip
    (copy(ps = updated ++ emps.flatten, emitters=ems), lights.flatten)
  }
  def ++(s:Seq[Particle]) = copy(ps = ps ++ s)

  def addEmitters(ems:Seq[Emitter]) = copy(emitters = emitters ++ ems)
  def draw(tr:TileRenderer):TileRenderer = {
    tr <++< ps.map { _.draw _ }
  }
}
