package in.dogue.profundus.entities

import in.dogue.antiqua.Antiqua
import Antiqua._

object Inventory {
  def create:Inventory = {
    val dura = 400
    Inventory(2, 2, 0, dura, dura)
  }
}

case class Inventory private (ropes:Int, bombs:Int, gems:Int, dura:Int, maxDura:Int) {
  def hasBomb = bombs > 0
  def spendBomb = copy(bombs = bombs.drop1)

  def hasRope = ropes > 0
  def spendRope = copy(ropes = ropes.drop1)

  def hasShovelUse = dura > 0
  def useShovel(damage:Int) = copy(dura=dura.drop(damage))
  def collect(g:MineralDrop) = copy(gems=gems+1)
}
