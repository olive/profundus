package in.dogue.profundus.procgen

import in.dogue.profundus.mode.loadout.Loadout
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.profundus.entities.{Mallet, Mattock, Shovel, Rapier}

object LoadoutTitle {
  def get(lo:Loadout):LoadoutTitle =  {
    All.find(s => s.satisfies(lo)).headOption.getOrElse(new LoadoutTitle {
      override def satisfies(lo: Loadout): Boolean = true
      override def name: String = "Simpleton"
    })
  }


  private def ropeMonger(lo:Loadout) = lo.ropes >= 6
  private def pyromancer(lo:Loadout) = lo.capsules >= 6 || lo.fuel > 40
  private def fencer(lo:Loadout) = lo.tool == Rapier
  private def thief(lo:Loadout) = lo.minerals > 60
  private def spelunker(lo:Loadout) = lo.ropes == 4 && lo.capsules == 4
  private def depraved(lo:Loadout) = {
    lo.ropes == 0 && lo.capsules == 0 && lo.minerals == 0 && lo.fuel == 0 && lo.tool == Shovel
  }
  private def miner(lo:Loadout) = lo.tool == Mattock
  private def blacksmith(lo:Loadout) = lo.capsules >= 3 && lo.tool == Mallet
  private val All = Vector(
    "Rope Monger" @@ ropeMonger _,
    "Pyromancer" @@ pyromancer _,
    "Fencer" @@ fencer _,
    "Thief" @@ thief _,
    "Spelunker" @@ spelunker _,
    "Depraved" @@ depraved _,
    "Miner" @@ miner _,
    "Blacksmith" @@ blacksmith _

  ).map { case (n, f) => new LoadoutTitle {
    def satisfies(lo:Loadout) = f(lo)
    def name = n
  }}

  /*case object RopeMonger extends LoadoutTitle {

  }*/

}

sealed trait LoadoutTitle {
  def satisfies(lo:Loadout):Boolean
  def name:String
}

