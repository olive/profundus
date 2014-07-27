package in.dogue.profundus.world

import in.dogue.profundus.entities.{EntityManager, Player}
import in.dogue.profundus.entities.pickups.Pickup
import in.dogue.antiqua.Antiqua.Cell
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.profundus.Profundus
import Profundus._
case class Transaction(adjust:Int, item:Pickup)(dump:Cell) {
  def apply(p:Player, em:EntityManager):((Player, EntityManager), Seq[GlobalMessage]) = {
    if (p.getMineralCount + adjust > 0) {
        val drop = item.setPos(dump)
        ((p.adjustMinerals(adjust), em.removePickup(item)), Seq(drop).gms)
    } else {
      ((p, em), Seq())
    }

  }

}
