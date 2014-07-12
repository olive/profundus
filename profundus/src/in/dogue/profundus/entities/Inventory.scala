package in.dogue.profundus.entities

import in.dogue.antiqua.Implicits
import Implicits._

object Inventory {
  def create:Inventory = {
    Inventory(2, 0)
  }
}

case class Inventory private (bombs:Int, gems:Int) {
  def hasBomb = bombs > 0
  def spendBomb = copy(bombs = bombs.drop1)

  def collect(g:MineralDrop) = copy(gems=gems+1)
}
