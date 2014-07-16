package in.dogue.profundus.entities

import in.dogue.antiqua.Antiqua._
import in.dogue.profundus.entities.KillZone
import in.dogue.profundus.entities.SingleTileKillZone

case class SingleTileKillZone(ij:Cell) {
  def update = this
  def isDone = true
  def contains(pq:Cell) = ij == pq
  def toKillZone:KillZone[SingleTileKillZone] = KillZone(_.update, _.isDone, _.contains, this)
}
