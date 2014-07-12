package in.dogue.profundus.particles

import in.dogue.antiqua.graphics.TileRenderer


object ParticleManager {
  def create = ParticleManager(Seq())
}

case class ParticleManager private (ps:Seq[Particle[_]]) {
  def update = copy(ps = ps.map{_.update}.flatten)
  def ++(s:Seq[Particle[_]]) = copy(ps = ps ++ s)
  def draw(tr:TileRenderer):TileRenderer = {
    tr <++< ps.map { _.draw _ }
  }
}
