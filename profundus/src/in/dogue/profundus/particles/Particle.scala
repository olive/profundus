package in.dogue.profundus.particles

import in.dogue.antiqua.graphics.TileRenderer
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.profundus.lighting.LightSource
import in.dogue.profundus.world.TerrainCache

object Particle {
  def apply[A](aup:A=>TerrainCache => A,
               adrawFunc:A => TileRenderer => TileRenderer,
               agetLight:A => Seq[LightSource],
               aisDone:A => Boolean,
               aself:A) = new Particle {
    override type T = A
    override val up = aup
    override val drawFunc = adrawFunc
    override val getLight = agetLight
    override val isDone = aisDone
    override val self = aself
  }
}

trait Particle {
  type T
  val up:T=>TerrainCache => T
  val drawFunc:T => TileRenderer => TileRenderer
  val getLight:T => Seq[LightSource]
  val isDone:T => Boolean
  val self:T

  private def updateSelf(tc:TerrainCache) = Particle(up, drawFunc, getLight, isDone, up(self)(tc))

  def update(tc:TerrainCache):Option[(Particle,Seq[LightSource])] = {
    if (isDone(self)) {
      None
    } else {
      val light = getLight(self)
      (updateSelf(tc), light).some
    }
  }
  def draw(tr:TileRenderer):TileRenderer = {
    tr <+< drawFunc(self)
  }
}
