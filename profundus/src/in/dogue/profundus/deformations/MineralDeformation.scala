package in.dogue.profundus.deformations

import in.dogue.antiqua.Antiqua._
import in.dogue.profundus.world.{GlobalSpawn, TerrainCache}
import in.dogue.profundus.entities.Mattock

object MineralDeformation {
  def create(ij:Cell, seed:Int) = {
    MineralDeformation(ij, seed)
  }
}
case class MineralDeformation private (ij:Cell, seed:Int) {
  final val i = ij.x
  final val j = ij.y
  def update = this
  def isDone = true

  def deform(tc:TerrainCache): (TerrainCache, Seq[GlobalSpawn], Int) = {
    tc.mineralize(ij, seed) @@ Seq() @@ 0
  }

  def toDeformation:Deformation = {
    Deformation[MineralDeformation](_.update, _.isDone, _.deform, this)
  }
}
