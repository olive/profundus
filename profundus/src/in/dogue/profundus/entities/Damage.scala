package in.dogue.profundus.entities

object DamageType {
  case object Obelisk extends DamageType
  case class Creature private[DamageType] (name:String) extends DamageType
  case class Environment private[DamageType] (name:String) extends DamageType
  case object Player extends DamageType
  val Lurker = Creature("Lurker")
  val HellBat = Creature("Hell Bat")
  val Fall = Environment("Fall")
  val Explosion = Environment("Explosion")

}

sealed trait DamageType


case class Damage(amount:Int, source:DamageType)
