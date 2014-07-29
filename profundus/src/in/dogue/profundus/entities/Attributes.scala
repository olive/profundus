package in.dogue.profundus.entities

import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.profundus.Game

object Attributes {
  def create = {
    val fly = if (Game.flyMode) Seq(Wings) else Seq()
    val halo = if (Game.invMode) Seq(Halo) else Seq()
    Attributes(10, 2, 100, 5, 5, 0, 12, 4, 0, 1, 1, fly ++ halo)
  }
  val default = create
}
case class Attributes(lightRadius:Double, flareMod:Int, maxHp:Int, stamRegen:Int, healthRegen:Int, toolRegen:Int, airMove:Int, groundMove:Int, gMod:Double, attack:Double, defense:Double, items:Seq[Item]) {
  def getRadius = lightRadius
  def incRadius(i:Double) = copy(lightRadius=lightRadius+i)
  def hasLongArms = items.contains(LongArms)
  def hasTrampoline = items.contains(Trampoline)
  def hasWings = items.contains(Wings)
  def hasHalo = items.contains(Halo)
  def fallDistance = hasTrampoline.select(7, 12)
  def getItems = items
  def restore(i:Int) = copy(maxHp = maxHp + i)
  def collectItem(it:Item) = copy(items = it +: items)
  def incStam(i:Int) = copy(stamRegen = stamRegen+i)
  def incHealthRegen(i:Int) = copy(healthRegen=healthRegen+i)
  def incAirMove(i:Int) = copy(airMove=airMove+i)
  def incGroundMove(i:Int) = copy(groundMove=groundMove+i)
  def incGravity(d:Double) = copy(gMod=gMod + d)
  def incAttack(d:Double) = copy(attack=attack + d)
  def incDefense(d:Double) = copy(defense=defense + d)
  def kill = copy(healthRegen = 0)
  def reduceDamage = { (d:Int) => math.ceil(d*defense).toInt}
  def multiplyDamage(dmg:Int) = math.ceil(dmg * attack).toInt
}
