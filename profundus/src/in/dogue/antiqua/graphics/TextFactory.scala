package in.dogue.antiqua.graphics

import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.data.Code


object TextFactory {
  val bw = TextFactory(Color.Black, Color.White)
}
case class TextFactory(bgColor:Color, fgColor:Color) {
  def withBg(c:Color) = copy(bgColor = c)
  def withFg(c:Color) = copy(fgColor = c)

  private def makeTiles(s:Vector[Code], bgColor:Color, fgColor:Color) = {
    s.map{c => Tile(c, bgColor, fgColor)}.toVector
  }

  def create(s:String) = {
    val tiles = makeTiles(s.map(Code.unicodeToCode).toVector, bgColor, fgColor)
    Text(tiles, this)
  }

  def fromCodes(s:Vector[Code]) = {
    val tiles = makeTiles(s, bgColor, fgColor)
    Text(tiles, this)
  }
}
