package in.dogue.profundus.world

import in.dogue.antiqua.graphics.{TileRenderer, Tile}
import in.dogue.antiqua.Antiqua
import Antiqua._
import com.deweyvm.gleany.graphics.Color
import in.dogue.profundus.entities.MineralDrop
import scala.collection.script.Update


sealed trait TileType {
  val tile:Tile
  val bg:Tile
  val isWalkable:Boolean = false
  type Update= (TileType, Seq[MineralDrop], Int, Boolean)
  def hit:((Int,Int), Int) => Update
  def standard(f:( (Int,Int), Int) => (TileType, Int, Boolean)): ((Int,Int), Int) => Update = { case (ij, dmg) =>
    val (tt, toolDmg, broke) = f(ij, dmg)
    (tt, Seq(), toolDmg, broke)
  }

  def toEmpty(toolDamage:Int, hp:Int, cop:Int=>TileType):((Int,Int),Int) => Update = standard { case (ij, dmg) =>
    val newHp =  hp.drop(dmg)
    val newTile = if (newHp > 0) {
      cop(newHp)
    } else {
      Empty(bg)
    }
    (newTile, toolDamage, newHp <= 0)
  }
  def getDrop = Seq()
  def getDamage = 1
}

case class Empty(override val tile:Tile) extends TileType {
  override val isWalkable = true
  override val bg = tile
  override def getDamage = 0
  override def hit = { case _ =>
    (this, getDrop, getDamage, false)
  }
}

object Rock3 { def create(t:Tile, bg:Tile) = Rock3(t, bg, 50) }
case class Rock3(override val tile:Tile, override val bg:Tile, hp:Int) extends TileType {
  val toolDamage = 50
  def setHp(i:Int) = copy(hp=i)
  override def hit = toEmpty(toolDamage, hp, setHp)
}

object Rock2 { def create(t:Tile, bg:Tile) = Rock2(t, bg, 15) }
case class Rock2(override val tile:Tile, override val bg:Tile, hp:Int) extends TileType {
  val toolDamage = 15
  def setHp(i:Int) = copy(hp=i)
  override def hit = toEmpty(toolDamage, hp, setHp)
}

object Rock { def create(t:Tile, bg:Tile) = Rock(t, bg, 5) }
case class Rock(override val tile:Tile, override val bg:Tile, hp:Int) extends TileType {
  val toolDamage = 5
  def setHp(i:Int) = copy(hp=i)
  override def hit = toEmpty(toolDamage, hp, setHp)
}

object Clay { def create(t:Tile, bg:Tile) = Clay(t, bg, 2) }
case class Clay(override val tile:Tile, override val bg:Tile, hp:Int) extends TileType {
  val toolDamage = 5
  def setHp(i:Int) = copy(hp=i)
  override def hit = toEmpty(toolDamage, hp, setHp)
}

object Dirt { def create(t:Tile, bg:Tile) = Dirt(t, bg, 1) }
case class Dirt(override val tile:Tile, override val bg:Tile, hp:Int) extends TileType {
  val toolDamage = 1
  def setHp(i:Int) = copy(hp=i)
  override def hit = toEmpty(toolDamage, hp, setHp)
}

object Mineral { def create(t:Tile, bg:Tile, c:Color) = Mineral(t, bg, c, 3) }
case class Mineral(override val tile:Tile, override val bg:Tile, c:Color, hp:Int) extends TileType {
  val toolDamage = 1
  def setHp(i:Int) = copy(hp=i)
  override def hit = toEmpty(toolDamage, hp, setHp)
}

case class WorldTile(state:TileType) {
  def tile = state.tile
  def draw(i:Int, j:Int)(tr:TileRenderer):TileRenderer = {
    val tile = state.tile
    tr <+ (i, j, tile)
  }
}
