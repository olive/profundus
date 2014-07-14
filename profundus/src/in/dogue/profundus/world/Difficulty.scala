package in.dogue.profundus.world

sealed trait Difficulty {
  val name:String
}
case object Easy extends Difficulty {
  override val name = "Well-trodden"
}
case object Normal extends Difficulty {
  override val name = "Abandoned"
}
case object Hard extends Difficulty{
  override val name = "Forgotten"
}
