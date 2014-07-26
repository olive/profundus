package in.dogue.profundus.entities

import in.dogue.antiqua.Antiqua.Cell

case class PlayerInfo(pos:Option[Cell], live:LivingState, numMinerals:Int)
