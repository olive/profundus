package in.dogue.profundus.deformations

import in.dogue.profundus.world.{TerrainCache, World}
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.profundus.entities.MineralDrop

case class Deformation[T](up:T => T,
                          done:T => Boolean,
                          deform:T=>TerrainCache => (TerrainCache, Seq[MineralDrop], Int),
                          self:T) {
  def apply(tr:TerrainCache):(TerrainCache, Seq[MineralDrop], Int) = deform(self)(tr)

  def update:Option[Deformation[A] forSome {type A}] = {
    if (done(self)) {
      None
    } else {
      copy(self=up(self)).some
    }
  }

}
