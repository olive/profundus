package in.dogue.profundus.mode.loadout

import in.dogue.antiqua.graphics.{TileRenderer, Border, TextFactory}
import in.dogue.profundus.Profundus
import in.dogue.antiqua.Antiqua._
import in.dogue.antiqua.data.CP437

object LoadoutSection {
  def create(cols:Int, rows:Int, ij:Cell, title:Vector[String], tr:TextFactory, offset:(Int,Int)) = {
    val border = Profundus.border(cols, rows)
    val text = tr.fromLines(title).sfilter(CP437.notBlank)
    LoadoutSection(ij, border, text, offset)
  }
}

case class LoadoutSection private (ij:Cell, b:Border, text:TileGroup, offset:(Int,Int)) {
  def draw(tr:TileRenderer):TileRenderer = {
    val off = ij |+| offset
    tr <+< b.draw(ij) <|| (text |+| off)
  }
}
