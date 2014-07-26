package in.dogue.profundus.deformations

import in.dogue.profundus.world.{GlobalMessage, TerrainCache}
import in.dogue.antiqua.Antiqua
import Antiqua._


object Deformation {
  def apply[A](aup:A => A,
               adone:A => Boolean,
               adeform:A=>TerrainCache => (TerrainCache, Seq[GlobalMessage], Int),
               aself:A) = new Deformation {
    override type T = A
    override val up = aup
    override val done = adone
    override val deform = adeform
    override val self = aself
  }
}

trait Deformation {
  type T
  val up:T => T
  val done:T => Boolean
  val deform:T=>TerrainCache => (TerrainCache, Seq[GlobalMessage], Int)
  val self:T

  def apply(tr:TerrainCache):(TerrainCache, Seq[GlobalMessage], Int) = deform(self)(tr)

  private def updateSelf = Deformation(up, done, deform, up(self))

  def update:Option[Deformation] = {
    if (done(self)) {
      None
    } else {
      updateSelf.some
    }
  }

}
