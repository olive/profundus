package in.dogue.profundus.entities


import in.dogue.antiqua.graphics.{TileRenderer, Tile}
import in.dogue.antiqua.data.CP437
import in.dogue.antiqua.Antiqua
import Antiqua._
import com.deweyvm.gleany.graphics.Color
import Color._

case class Tool(dura:Int, `type`:ToolType) {
  def damage(amt:Int) = copy(dura=dura.drop(amt))
  def draw(i:Int, j:Int)(tr:TileRenderer):TileRenderer = {
    tr <++ (`type`.icon |+| (i, j))
  }
}





sealed trait ToolType {
  val durability:Int
  val digDamage:Int
  val icon:TileGroup
  def toTool = Tool(durability, this)
}
case object Shovel extends ToolType {
  override val durability = 20
  override val digDamage = 3
  override val icon = Tile.makeGroup(Vector(
    (0, 0, CP437.`[`, Black, Brown),
    (1, 0, CP437.`─`, Black, Brown),
    (2, 0, CP437.`─`, Black, Brown),
    (3, 0, CP437.`D`, Black, Grey)
  ))

}
case object Mallet extends ToolType {
  override val durability = 2000
  override val digDamage = 5
  override val icon = Tile.makeGroup(Vector(
    (1, 0, CP437.`─`, Black, Brown),
    (2, 0, CP437.`─`, Black, Brown),
    (3, 0, CP437.▌, Black, DarkGrey)
  ))
}
case object Mattock extends ToolType {
  override val durability = 500
  override val digDamage = 15
  override val icon = Tile.makeGroup(Vector(
    (1, 0, CP437.`─`, Black, Brown),
    (2, 0, CP437.`─`, Black, Brown),
    (3, 0, CP437.`}`, Black, Grey)
  ))
}
case object Rapier extends ToolType {
  override val durability = 100
  override val digDamage = 1
  override val icon = Tile.makeGroup(Vector(
    (0, 0, `CP437`.┼, Black, Brown),
    (1, 0, CP437.`─`, Black, Grey),
    (2, 0, CP437.`─`, Black, Grey),
    (3, 0, CP437.`─`, Black, Grey)
  ))
}
