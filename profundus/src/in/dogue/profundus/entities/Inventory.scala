package in.dogue.profundus.entities

import in.dogue.antiqua.Implicits
import Implicits._

object Inventory {
  def create:Inventory = {
    Inventory(2)
  }
}

case class Inventory private (bombs:Int) {
  def hasBomb = bombs > 0
  def spendBomb = copy(bombs = bombs.drop1)
}
