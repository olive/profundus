package in.dogue.antiqua.graphics

import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.data.CP437

case class TileFactory(bg:Color, fg:Color) {
  def apply(c:CP437) = c.mkTile(bg, fg)
}
