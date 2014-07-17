package in.dogue.profundus.entities.killzones

import in.dogue.antiqua.Antiqua
import Antiqua._

object ExplosionKillZone {
  def create(i:Int, j:Int, radius:Int, speed:Int) = {
    ExplosionKillZone(i, j, radius, speed, 0)
  }
}
case class ExplosionKillZone private (i:Int, j:Int, radius:Int, speed:Int, t:Int) {
  def update = copy(t=t+1)
  def isDone = t > radius*speed
  def contains(ij:Cell) = {
    scala.math.hypot(ij.x - i, ij.y - j) < t/speed
  }
  def toKillZone:KillZone[ExplosionKillZone] = {
    KillZone(_.update, _.isDone, _.contains, this)
  }
}
