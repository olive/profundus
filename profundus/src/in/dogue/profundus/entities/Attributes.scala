package in.dogue.profundus.entities

import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.profundus.Game

object Attributes {
  def create = {
    val fly = if (Game.flyMode) Seq(Wings) else Seq()
    val halo = if (Game.invMode) Seq(Halo) else Seq(LongArms)
    Attributes(5, 5, 0, fly ++ halo)
  }
  val default = create
}
case class Attributes(stamRegen:Int, healthRegen:Int, toolRegen:Int, items:Seq[Item]) {
  def hasLongArms = items.contains(LongArms)
  def hasTrampoline = items.contains(Trampoline)
  def hasWings = items.contains(Wings)
  def hasHalo = items.contains(Halo)
  def fallDistance = hasTrampoline.select(7, 12)
  def getItems = items
  def collectItem(it:Item) = copy(items = it +: items)
}
