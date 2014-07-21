package in.dogue.profundus.particles

import in.dogue.antiqua.graphics.TileRenderer
import in.dogue.profundus.lighting.LightSource
import in.dogue.profundus.world.TerrainCache


object ParticleManager {
  def create = {
    ParticleManager(Seq(), Seq())
  }
}

case class ParticleManager private (emitters:Seq[Emitter[_]], ps:Seq[Particle[_]]) {
  def update(tc:TerrainCache) = {
    val (ems, emps) = emitters.map {e =>
      e.update : (Option[(Emitter[_$1] forSome {type _$1}, Seq[Particle[_]])])
    }.flatten.unzip
    val (updated, lights) = ps.map{_.update(tc)}.flatten.unzip
    (copy(ps = updated ++ emps.flatten, emitters=ems), lights.flatten)
  }
  def ++(s:Seq[Particle[_]]) = copy(ps = ps ++ s)

  def addEmitters(ems:Seq[Emitter[_]]) = copy(emitters = emitters ++ ems)
  def draw(tr:TileRenderer):TileRenderer = {
    tr <++< ps.map { _.draw _ }
  }
}
