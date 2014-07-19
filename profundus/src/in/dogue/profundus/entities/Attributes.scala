package in.dogue.profundus.entities

import in.dogue.antiqua.Antiqua
import Antiqua._

object Attributes {
  def create = Attributes(5, 5, 0, Seq())
  val default = create
}
case class Attributes(stamRegen:Int, healthRegen:Int, toolRegen:Int, items:Seq[Item]) {
  def hasLongArms = items.contains(LongArms)
  def hasTrampoline = items.contains(Trampoline)

  def fallDistance = hasTrampoline.select(6, 12)
  def getItems = items
  def collectItem(it:Item) = copy(items = it +: items)
}
