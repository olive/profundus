package in.dogue.profundus.entities


import in.dogue.antiqua.graphics.{TileRenderer, Tile}
import in.dogue.antiqua.data.CP437
import in.dogue.antiqua.Antiqua
import Antiqua._
import com.deweyvm.gleany.graphics.Color
import Color._
import in.dogue.profundus.world.TileClass
import scala.util.Random

case class Tool(dura:Int, `type`:ToolType) {
  def isBare = `type` == BareHands
  def damage(amt:Int) = copy(dura=dura.drop(amt))
  def draw(ij:Cell)(tr:TileRenderer):TileRenderer = {
    tr <++ (`type`.icon |++| ij)
  }
}

object ToolType {
  val All = Vector(Gouge, Shovel, Mallet, Mattock, Rapier)
  def randomR(r:Random) = All.randomR(r)
}

sealed trait ToolType {
  val healthHurt:Int = 0
  val durability:Int
  val digDamage:Int
  val stamCost:Int
  val breakable:Seq[TileClass]
  val icon:TileGroup
  def toTool = Tool(durability, this)
}

case object BareHands extends ToolType {
  override val healthHurt = 1
  override val durability = Int.MaxValue
  override val digDamage = 1
  override val stamCost = 10
  override val breakable = Seq(TileClass.Dirt)
  override val icon = Tile.makeGroup(Vector(
    (1, 0, CP437.º, Black, Brown),
    (2, 0, CP437.☻, Black, Brown),
    (3, 0, CP437.º, Black, Brown)
  ))

}

case object Drill extends ToolType {
  override val durability = 50000
  override val digDamage = 100
  override val stamCost = 1
  override val breakable = Seq(TileClass.Dirt, TileClass.Clay, TileClass.Rock1, TileClass.Rock2, TileClass.Rock3)
  override val icon = Tile.makeGroup(Vector(
    (1, 0, CP437.`[`, Black, Grey),
    (2, 0, CP437.≡, Black, Grey),
    (3, 0, CP437.►, Black, Grey)
  ))

}

case object Gouge extends ToolType {
  override val durability = 5000
  override val digDamage = 1
  override val stamCost = 3
  override val breakable = Seq(TileClass.Dirt)
  override val icon = Tile.makeGroup(Vector(
    (1, 0, CP437.═, Black, Brown),
    (2, 0, CP437.═, Black, Brown),
    (3, 0, CP437.Ç_u, Black, Grey)
  ))
}

case object Shovel extends ToolType {
  override val durability = 1000
  override val digDamage = 3
  override val stamCost = 8
  override val breakable = Seq(TileClass.Dirt, TileClass.Clay)
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
  override val stamCost = 15
  override val breakable = Seq(TileClass.Dirt, TileClass.Rock1, TileClass.Rock2)
  override val icon = Tile.makeGroup(Vector(
    (1, 0, CP437.`─`, Black, Brown),
    (2, 0, CP437.`─`, Black, Brown),
    (3, 0, CP437.▌, Black, DarkGrey)
  ))
}
case object Mattock extends ToolType {
  override val durability = 500
  override val digDamage = 5
  override val stamCost = 10
  override val breakable = Seq(TileClass.Dirt, TileClass.Clay, TileClass.Rock1, TileClass.Rock2, TileClass.Rock3)
  override val icon = Tile.makeGroup(Vector(
    (1, 0, CP437.`─`, Black, Brown),
    (2, 0, CP437.`─`, Black, Brown),
    (3, 0, CP437.`}`, Black, Grey)
  ))
}
case object Rapier extends ToolType {
  override val durability = 100
  override val digDamage = 1
  override val stamCost = 5
  override val breakable = Seq(TileClass.Dirt)
  override val icon = Tile.makeGroup(Vector(
    (0, 0, `CP437`.┼, Black, Brown),
    (1, 0, CP437.`─`, Black, Grey),
    (2, 0, CP437.`─`, Black, Grey),
    (3, 0, CP437.`─`, Black, Grey)
  ))
}
