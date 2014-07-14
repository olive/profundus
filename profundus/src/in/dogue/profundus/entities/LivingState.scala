package in.dogue.profundus.entities


sealed trait LivingState
case object Alive extends LivingState
case object Dead extends LivingState
