package in.dogue.profundus.entities

import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.profundus.Game

sealed trait FallState {
  /** number of tiles you have been falling for*/
  val tiles:Int
  val moveSlow:Boolean
}

object Falling { def create = Falling(0, 0) }
case class Falling(t:Int, override val tiles:Int) extends FallState {
  val v0 = 8
  val g = 9.8
  def sq(i:Double) = i*i
  def t(tiles:Int) = -v0 / g + Math.sqrt(sq(v0) / sq(g) + 2*tiles / g)
  def fallTime(tiles:Int): Int /* in frames */  = {
    ((t(tiles+1) - t(tiles))*60).toInt
  }
  override val moveSlow = true
}
case object Grounded extends FallState {
  override val tiles = 0
  override val moveSlow = false
}
case object Floating extends FallState {
  override val tiles = 0
  override val moveSlow = false
}
