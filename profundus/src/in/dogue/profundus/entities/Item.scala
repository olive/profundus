package in.dogue.profundus.entities

import in.dogue.antiqua.graphics.Tile
import in.dogue.antiqua.data.CP437
import in.dogue.antiqua.Antiqua
import Antiqua._
import com.deweyvm.gleany.graphics.Color

sealed trait Item {
  val icon:Tile
}

case object LongArms extends Item {
  override val icon = CP437.¬.mkTile(Color.Black, Color.Tan)
}

case object Trampoline extends Item {
  override val icon = CP437.∩.mkTile(Color.Black, Color.Purple)
}

case object Wings extends Item {
  override val icon = CP437.∞.mkTile(Color.Black, Color.White)
}

case object Halo extends Item {
  override val icon = CP437.°.mkTile(Color.Black, Color.Yellow)
}
