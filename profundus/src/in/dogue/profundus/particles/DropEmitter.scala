package in.dogue.profundus.particles

import in.dogue.antiqua.Antiqua.Cell

object DropEmitter {
  def create(ij:Cell, freq:Int, tStart:Int) = {
    DropEmitter(ij, freq, tStart)
  }
}

case class DropEmitter private (ij:Cell, freq:Int, t:Int) {
  def update = {
    val drop = if (t % freq == 0) {
      Seq(WaterDrop.create(ij).toParticle)
    } else {
      Seq()
    }
    (copy(t=t+1), drop)
  }

  def isDone = false

  def toEmitter:Emitter = Emitter[DropEmitter](_.update, _.isDone, this)
}
