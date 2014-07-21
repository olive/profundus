package in.dogue.profundus.entities

import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.profundus.mode.loadout.Loadout
import in.dogue.profundus.entities.pickups.{RopePickup, MineralPickup}

object Inventory {
  def create(lo:Loadout):Inventory = {
    Inventory(lo.fuel, lo.ropes, lo.bombs, lo.gems, lo.`type`.toTool)
  }
}

case class Inventory private (fuel:Int, ropes:Int, bombs:Int, gems:Int, tool:Tool) {
  def hasBomb = bombs > 0
  def spendBomb = copy(bombs = bombs.drop1)

  def hasRope = ropes > 0
  def spendRope = copy(ropes = ropes.drop1)

  def hasFlare = fuel > 0
  def useFlare = copy(fuel=fuel.drop1)

  def useTool(dmg:Int) = copy(tool=tool.damage(dmg))
  def setTool(t:Tool) = copy(tool=t)

  def collectMineral(g:MineralPickup) = copy(gems=gems+1)
  def collectRope(g:RopePickup) = copy(ropes=ropes+1)
}
