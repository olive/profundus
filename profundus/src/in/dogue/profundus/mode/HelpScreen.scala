package in.dogue.profundus.mode

import in.dogue.antiqua.Antiqua.TileGroup
import in.dogue.antiqua.graphics.TileRenderer
import in.dogue.profundus.Profundus
import Profundus._
import in.dogue.antiqua.Antiqua
import Antiqua._


object HelpScreen {
  def create = {
    val tf = Profundus.tf
    val movement = tf.create("Movement ↑↓→←").toTileGroup |++| ((1,1))
    HelpScreen(Seq(movement))
  }

}

case class HelpScreen(grs:Seq[TileGroup]) {
  def draw(tr:TileRenderer):TileRenderer = {
    tr <+++ grs
  }
}
