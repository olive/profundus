package in.dogue.profundus.entities

object DamageType {
  case object Obelisk extends DamageType
  case object Creature extends DamageType
  case object Environment extends DamageType
  case object Player extends DamageType
}

sealed trait DamageType


case class Damage(amount:Int, source:DamageType)
