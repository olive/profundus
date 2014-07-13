package in.dogue.profundus.entities

sealed trait FallState {
  /** number of tiles you have been falling for*/
  val tiles:Int
}

object Falling { def create = Falling(0, 0) }
case class Falling(t:Int, override val tiles:Int) extends FallState {
  val fallTime = 6
}
case object Grounded extends FallState {
  override val tiles = 0
}
case object Floating extends FallState {
  override val tiles = 0
}
