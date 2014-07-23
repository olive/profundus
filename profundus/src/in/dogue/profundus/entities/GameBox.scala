package in.dogue.profundus.entities

import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.antiqua.graphics.TileRenderer
import in.dogue.profundus.ui.{MessageBoxContinue, MessageBoxComplete, MessageBox}

case class GameBox(ij:Cell, mb:MessageBox[Unit]) {
  def update:Option[GameBox] = {
    mb.update match {
      case MessageBoxComplete(f) => None
      case MessageBoxContinue(box) => copy(mb=box).some
    }
  }
  def draw(tr:TileRenderer):TileRenderer = {
    tr <+< mb.draw(ij)
  }
}
