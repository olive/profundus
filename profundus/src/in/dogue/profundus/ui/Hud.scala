package in.dogue.profundus.ui

import in.dogue.antiqua.graphics._
import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.data.CP437
import in.dogue.antiqua.graphics.Text
import in.dogue.antiqua.graphics.Tile

object Hud {
  def create(cols:Int, rows:Int):Hud = {
    val rect = Rect.createPlain(cols, rows, CP437.` `.mkTile(Color.Black, Color.White))
    val tf = TextFactory(Color.Black, Color.White)
    Hud(cols, rect, tf.create("This is a test"), tf.create("Depth:"), tf.create("0"), tf)
  }
}

case class Hud private (height:Int, rect:Rect, text:Text, depth:Text, depthAmt:Text, tf:TextFactory) {
  def atDepth(i:Int) = copy(depthAmt=tf.create(i.toString))
  def draw(tr:TileRenderer):TileRenderer = {
    tr <+< rect.draw(0,0) <+< text.draw(1, 1) <+< depth.draw(1, 2) <+< depthAmt.draw(8, 2)
  }
}
