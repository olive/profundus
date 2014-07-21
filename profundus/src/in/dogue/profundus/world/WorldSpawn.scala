package in.dogue.profundus.world

import in.dogue.profundus.entities.{Entity, Obelisk, Lurker}
import in.dogue.profundus.entities.pickups.{FoodPickup, Pickup}
import in.dogue.profundus.doodads.Doodad

sealed trait WorldSpawn
case class EntitySpawn(s:Seq[Entity[_]]) extends WorldSpawn
case class PickupSpawn(s:Seq[Pickup[_]]) extends WorldSpawn
