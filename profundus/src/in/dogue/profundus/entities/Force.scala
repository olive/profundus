package in.dogue.profundus.entities

import in.dogue.profundus.world.TerrainCache
import in.dogue.antiqua.Antiqua.Cell
import in.dogue.antiqua.data.Direction
import in.dogue.antiqua.Antiqua
import Antiqua._

object Force {
  def constForce(amt:Int, time:Int, dir:Direction, t:Int):Force = new Force {
    override def affect(f:FallState, pos: Cell, cache: TerrainCache): Option[(Cell, Direction)] = {
        (pos --> dir, dir).onlyIf(t % time == 0 && !isDone(f))
    }
    override def isDone(f:FallState): Boolean = t >= time*amt || f == Grounded
    override def update: Force = constForce(amt, time, dir, t+1)
  }

  def mkClimbForce(vec:Vector[Direction], time:Int) = climbForce(vec, 0, time, 0)
  private def climbForce(vec:Vector[Direction], ptr:Int, time:Int, t:Int):Force = new Force {
    override def affect(f:FallState, pos: Cell, cache: TerrainCache): Option[(Cell, Direction)] = {
      (pos --> vec(ptr), vec(ptr)).onlyIf(t > 0 && t % time == 0 && !isDone(f))
    }
    override def isDone(f:FallState): Boolean = ptr == vec.length
    override def update: Force = {
      val newPtr = ptr + (t > 0 && t % time == 0).select(0, 1)

      climbForce(vec, newPtr, time, t+1)
    }
  }

}


trait Force {
  def update:Force
  def isDone(f:FallState):Boolean
  def affect(f:FallState, pos:Cell, cache:TerrainCache):Option[(Cell, Direction)]
}
