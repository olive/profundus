package in.dogue.profundus.particles

import in.dogue.antiqua.graphics.TileRenderer
import in.dogue.profundus.lighting.LightSource


object ParticleManager {
  def create = ParticleManager(Seq())
}

case class ParticleManager private (ps:Seq[Particle[_]]) {
  def update = {
    val (updated, lights) = ps.map{_.update}.flatten.unzip
    (copy(ps = updated), lights.flatten)
  }
  def ++(s:Seq[Particle[_]]) = copy(ps = ps ++ s)
  def draw(tr:TileRenderer):TileRenderer = {
    tr <++< ps.map { _.draw _ }
  }
}
