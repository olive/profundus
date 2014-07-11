package in.dogue.antiqua.graphics

import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.data.CP437


object TextFactory {
  val bw = TextFactory(Color.Black, Color.White)
}
case class TextFactory(bgColor:Color, fgColor:Color) {
  def withBg(c:Color) = copy(bgColor = c)
  def withFg(c:Color) = copy(fgColor = c)

  private def makeTiles(s:Vector[CP437], bgColor:Color, fgColor:Color) = {
    s.map{_.mkTile(bgColor, fgColor)}.toVector
  }

  def create(s:String) = {
    val tiles = makeTiles(s.map(CP437.unicodeToCode).toVector, bgColor, fgColor)
    Text(tiles, this)
  }

  def fromCodes(s:Vector[CP437]) = {
    val tiles = makeTiles(s, bgColor, fgColor)
    Text(tiles, this)
  }
}
