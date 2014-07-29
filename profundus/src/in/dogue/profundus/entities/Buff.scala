package in.dogue.profundus.entities

import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.data.Code
import in.dogue.profundus.entities.pickups.FoodType


case class Buff(icon:Code, color:Color, typ:FoodType, process:Attributes => Attributes) {
  def doProcess(attr:Attributes) = process(attr)
  def getIcon = icon.mkTile(Color.Black, color)
}



