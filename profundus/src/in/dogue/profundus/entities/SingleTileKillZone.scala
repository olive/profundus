package in.dogue.profundus.entities

case class SingleTileKillZone(ij:(Int,Int)) {
  def update = this
  def isDone = true
  def contains(pq:(Int,Int)) = ij == pq
  def toKillZone:KillZone[SingleTileKillZone] = KillZone(_.update, _.isDone, _.contains, this)
}
