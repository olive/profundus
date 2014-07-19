package in.dogue.profundus.world

import in.dogue.profundus.entities.{Casque, Creature}
import in.dogue.profundus.entities.pickups.{FoodPickup, Pickup}

sealed trait WorldSpawn
case class CreatureSpawn(s:Seq[Creature]) extends WorldSpawn
case class PickupSpawn(s:Seq[Pickup[_]]) extends WorldSpawn
case class CasqueSpawn(s:Seq[Casque]) extends WorldSpawn
