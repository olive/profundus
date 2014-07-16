package in.dogue.profundus.mode.loadout

import in.dogue.antiqua.graphics.{TileRenderer, Border, TextFactory}
import in.dogue.profundus.Profundus
import in.dogue.antiqua.Antiqua._

object LoadoutSection {
  def create(cols:Int, rows:Int, i:Int, j:Int, title:Vector[String], tr:TextFactory, offset:(Int,Int)) = {
    val border = Profundus.border(cols, rows)
    val text = tr.fromLines(title)
    LoadoutSection(i, j, border, text, offset)
  }
}

case class LoadoutSection private (i:Int, j:Int, b:Border, text:TileGroup, offset:(Int,Int)) {
  def draw(tr:TileRenderer):TileRenderer = {
    val off = (i, j) |+| offset
    tr <+< b.draw(i, j) <++ (text |+| (off.x, off.y))
  }
}
