package in.dogue.profundus.particles

import in.dogue.antiqua.graphics.TileRenderer
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.profundus.lighting.LightSource
import in.dogue.profundus.world.TerrainCache

case class Particle[T](up:T=>TerrainCache => T,
                       drawFunc:T => TileRenderer => TileRenderer,
                       getLight:T => Seq[LightSource],
                       isDone:T => Boolean,
                       self:T) {
  def update(tc:TerrainCache):Option[(Particle[_] forSome {type A},Seq[LightSource])] = {
    if (isDone(self)) {
      None
    } else {
      val light = getLight(self)
      (copy(self=up(self)(tc)), light).some
    }
  }
  def draw(tr:TileRenderer):TileRenderer = {
    tr <+< drawFunc(self)
  }
}
