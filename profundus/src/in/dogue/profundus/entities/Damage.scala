package in.dogue.profundus.entities

object DamageType {
  case object Obelisk extends DamageType {
    def message = "a horrible device"
  }
  case class Creature private[DamageType] (art:String, name:String) extends DamageType {
    def message = art + " " + name
  }
  case class Environment private[DamageType] (name:String, msg:String) extends DamageType {
    def message = msg
  }
  case object Player extends DamageType {
    def message = "your own hand"
  }
  case object Tool extends DamageType {
    def message = "some tool"
  }
  val Lurker = Creature("a", "Lurker")
  val HellBat = Creature("a", "Hell Bat")
  val PhaseWasp = Creature("a", "Phase Wasp")
  val Bee = Creature("a", "large Bee")
  val Witness = Creature("a", "Witness")
  val Beezle = Creature("a", "Beezle")
  val Phoebe = Creature("the", "Phoebe")
  val Fall = Environment("Fall", "a great fall")
  val Explosion = Environment("Explosion", "an explosion")
  val Spikes = Environment("Explosion", "impalement")
  case object Unknown extends DamageType {
    def message = "forces unknown"
  }
}

sealed trait DamageType {
  def message:String
}


case class Damage(amount:Int, source:DamageType) {
  def reduce(f:Int=>Int): Damage = copy(amount = f(amount))
}
