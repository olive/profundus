package in.dogue.profundus.entities

import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.graphics.Tile
import in.dogue.antiqua.data.CP437
import in.dogue.antiqua.Antiqua
import Antiqua._

class FallIndicator(ij:Cell, fall:Int, fallThres:Int, state:LivingState) {
  private def getColor(fall:Int):Color = {
    Color.White.mix(Color.Red.dim(2), ((fall - fallThres)/(2*fallThres.toDouble)).clamp(0,1))
  }
  private def getIcon(fall:Int) = {
    if (fall > 2*fallThres) CP437.`‼` else CP437.!
  }
  def render:Option[(Cell, Tile)] = {
    val tile = getIcon(fall).mkTile(Color.Black, getColor(fall))
    (ij -| 1, tile).onlyIf(fall >= fallThres && state == Alive)
  }
}
