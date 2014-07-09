package in.dogue.profundus

import in.dogue.antiqua.data.Code
import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.graphics.{TileRenderer, Tile}
import in.dogue.profundus.input.Controls

object ExampleMode {
  def create(i:Int, j:Int) = {
    ExampleMode(i, j, Tile(Code.`@`, Color.Black, Color.White))
  }
}

case class ExampleMode private(i:Int, j:Int, p:Tile) {
  def update = {
    val newI = i + Controls.AxisX.justPressed
    val newJ = j + Controls.AxisY.justPressed
    copy(i=newI, j=newJ)
  }
  def draw(tr:TileRenderer):TileRenderer = {
    tr <+ (i, j, p)
  }
}
