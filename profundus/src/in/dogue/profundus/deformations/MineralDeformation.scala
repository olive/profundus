package in.dogue.profundus.deformations

import in.dogue.antiqua.Antiqua._
import in.dogue.profundus.world.{GlobalMessage, TerrainCache}
import in.dogue.profundus.entities.Mattock

object MineralDeformation {
  def create(ij:Cell) = {
    MineralDeformation(ij)
  }
}
case class MineralDeformation private (ij:Cell) {
  final val i = ij.x
  final val j = ij.y
  def update = this
  def isDone = true

  def deform(tc:TerrainCache): (TerrainCache, Seq[GlobalMessage], Int) = {
    tc.mineralize(ij) @@ Seq() @@ 0
  }

  def toDeformation:Deformation = {
    Deformation[MineralDeformation](_.update, _.isDone, _.deform, this)
  }
}
