package in.dogue.profundus.world

import in.dogue.profundus.entities.Creature
import in.dogue.profundus.entities.pickups.{FoodPickup, Pickup}

sealed trait WorldSpawn
case class CreatureSpawn(s:Seq[Creature]) extends WorldSpawn
case class FoodSpawn(s:Seq[Pickup[FoodPickup]]) extends WorldSpawn
