package in.dogue.profundus.entities

import in.dogue.profundus.world.TerrainCache
import in.dogue.antiqua.Antiqua.Cell
import in.dogue.antiqua.data.Direction
import in.dogue.antiqua.Antiqua
import Antiqua._

object Force {
  def constForce(amt:Int, time:Int, dir:Direction, t:Int):Force = {
    new Force {
      override def affect(pos: Cell, cache: TerrainCache): Option[(Cell, Direction)] = {
          (pos --> dir, dir).onlyIf(t % time == 0 && !isDone)
      }
      override def isDone: Boolean = t >= time*amt
      override def update: Force = constForce(amt, time, dir, t+1)
    }
  }
}


trait Force {
  def update:Force
  def isDone:Boolean
  def affect(pos:Cell, cache:TerrainCache):Option[(Cell, Direction)]
}
