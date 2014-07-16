package in.dogue.profundus.world

import in.dogue.antiqua.graphics.{TileRenderer, Tile}
import in.dogue.antiqua.Antiqua
import Antiqua._
import com.deweyvm.gleany.graphics.Color
import in.dogue.profundus.entities.MineralDrop

sealed trait TileType {
  val tile:Tile
  val bg:Tile
  val isWalkable:Boolean = false
  def hit(ij:(Int,Int), dmg:Int):(TileType, Seq[MineralDrop], Int, Boolean)
  def getDrop = Seq()
  def getDamage = 1
}

case class Empty(override val tile:Tile) extends TileType {
  override val isWalkable = true
  override val bg = tile
  override def getDamage = 0
  override def hit(ij:(Int,Int), dmg:Int) = (this, getDrop, getDamage, false)
}

object Rock3 {
  def create(t:Tile, bg:Tile) = Rock3(t, bg, 50)
}

case class Rock3(override val tile:Tile, override val bg:Tile, hp:Int) extends TileType {
  override def hit(ij:(Int,Int), dmg:Int) = if (hp > 1) {
    (copy(hp = hp.drop(dmg)), Seq(), 50, false)
  } else {
    (Empty(bg), Seq(), 50, true)
  }
}

object Rock2 {
  def create(t:Tile, bg:Tile) = Rock2(t, bg, 15)
}

case class Rock2(override val tile:Tile, override val bg:Tile, hp:Int) extends TileType {
  override def hit(ij:(Int,Int), dmg:Int) = if (hp > 1) {
    (copy(hp = hp.drop(dmg)), Seq(), 15, false)
  } else {
    (Empty(bg), Seq(), 15, true)
  }
}


object Rock {
  def create(t:Tile, bg:Tile) = Rock(t, bg, 5)
}
case class Rock(override val tile:Tile, override val bg:Tile, hp:Int) extends TileType {
  override def hit(ij:(Int,Int), dmg:Int) = if (hp > 1) {
    (copy(hp = hp.drop(dmg)), Seq(), 5, false)
  } else {
    (Empty(bg), Seq(), 5, true)
  }
}

object Clay {
  def create(t:Tile, bg:Tile) = Clay(t, bg, 2)
}

case class Clay(override val tile:Tile, override val bg:Tile, hp:Int) extends TileType {
  override def hit(ij:(Int,Int), dmg:Int) = if (hp > 1) {
    (copy(hp = hp.drop(dmg)), Seq(), 5, false)
  } else {
    (Empty(bg), Seq(), 5, true)
  }
}

object Dirt {
  def create(t:Tile, bg:Tile) = Dirt(t, bg, 1)
}
case class Dirt(override val tile:Tile, override val bg:Tile, hp:Int) extends TileType {
  override def hit(ij:(Int,Int), dmg:Int) = if (hp > 1) {
    (copy(hp = hp.drop(dmg)), Seq(), 1, false)
  } else {
    (Empty(bg), Seq(), 1, true)
  }
}
object Mineral {
  def create(t:Tile, bg:Tile, c:Color) = Mineral(t, bg, c, 3)
}
case class Mineral(override val tile:Tile, override val bg:Tile, c:Color, hp:Int) extends TileType {
  override def hit(ij:(Int,Int), dmg:Int) = if (hp > 1) {
    (copy(hp=hp.drop(dmg)), Seq(), 1, false)
  } else {
    (Empty(bg), Seq(MineralDrop.create(ij, c)), 1, true)
  }
}

case class WorldTile(state:TileType) {
  def tile = state.tile
  def draw(i:Int, j:Int)(tr:TileRenderer):TileRenderer = {
    val tile = state.tile
    tr <+ (i, j, tile)
  }
}
