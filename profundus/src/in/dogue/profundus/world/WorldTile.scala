package in.dogue.profundus.world

import in.dogue.antiqua.graphics.{TileRenderer, Tile}
import in.dogue.antiqua.Antiqua
import Antiqua._
import com.deweyvm.gleany.graphics.Color
import scala.collection.script.Update
import in.dogue.antiqua.data.Direction
import in.dogue.profundus.entities.pickups.{Pickup, MineralPickup}
import in.dogue.profundus.Profundus

object TileClass {
  case object Dirt extends TileClass
  case object Clay extends TileClass
  case object Rock extends TileClass
  case object Invincible extends TileClass
}
sealed trait TileClass


sealed trait TileType {
  val tile:Tile
  val tileClass:TileClass
  val bg:Tile
  val isWalkable:Boolean = false
  type Update= (TileType, Seq[WorldSpawn], Int, Boolean)
  def hit:(Cell, Int) => Update
  def standard(f:(Cell, Int) => (TileType, Int, Boolean)): (Cell, Int) => Update = { case (ij, dmg) =>
    val (tt, toolDmg, broke) = f(ij, dmg)
    (tt, Seq(), toolDmg, broke)
  }

  def toEmpty(toolDamage:Int, hp:Int, cop:Int=>TileType):(Cell,Int) => Update = standard { case (ij, dmg) =>
    val newHp =  hp.drop(dmg)
    val newTile = if (newHp > 0) {
      cop(newHp)
    } else {
      Empty(bg, true)
    }
    (newTile, toolDamage, newHp <= 0)
  }

  val bgSolid = true
}

case class Empty(override val tile:Tile, override val bgSolid:Boolean) extends TileType {
  override val isWalkable = true
  override val tileClass = TileClass.Invincible
  override val bg = tile
  override def hit = { case _ =>
    (this, Seq(), 0, false)
  }
}

object Shaft { def create(t:Tile) = Shaft(t) }
case class Shaft private (override val tile:Tile) extends TileType {
  override val bg = tile
  override val tileClass = TileClass.Invincible
  override def hit = { case _ =>
    (this, Seq(), 1, false)
  }
}

object Rock3 { def create(t:Tile, bg:Tile) = Rock3(t, bg, 50) }
case class Rock3(override val tile:Tile, override val bg:Tile, hp:Int) extends TileType {
  val toolDamage = 50
  override val tileClass = TileClass.Rock
  def setHp(i:Int) = copy(hp=i)
  override def hit = toEmpty(toolDamage, hp, setHp)
}

object Rock2 { def create(t:Tile, bg:Tile) = Rock2(t, bg, 15) }
case class Rock2(override val tile:Tile, override val bg:Tile, hp:Int) extends TileType {
  val toolDamage = 15
  override val tileClass = TileClass.Rock
  def setHp(i:Int) = copy(hp=i)
  override def hit = toEmpty(toolDamage, hp, setHp)
}

object Rock { def create(t:Tile, bg:Tile) = Rock(t, bg, 5) }
case class Rock(override val tile:Tile, override val bg:Tile, hp:Int) extends TileType {
  val toolDamage = 5
  override val tileClass = TileClass.Rock
  def setHp(i:Int) = copy(hp=i)
  override def hit = toEmpty(toolDamage, hp, setHp)
}

object Clay { def create(t:Tile, bg:Tile) = Clay(t, bg, 2) }
case class Clay(override val tile:Tile, override val bg:Tile, hp:Int) extends TileType {
  val toolDamage = 5
  override val tileClass = TileClass.Clay
  def setHp(i:Int) = copy(hp=i)
  override def hit = toEmpty(toolDamage, hp, setHp)
}

object Dirt { def create(t:Tile, bg:Tile) = Dirt(t, bg, 1) }
case class Dirt(override val tile:Tile, override val bg:Tile, hp:Int) extends TileType {
  val toolDamage = 1
  override val tileClass = TileClass.Dirt
  def setHp(i:Int) = copy(hp=i)
  override def hit = toEmpty(toolDamage, hp, setHp)
}

object Mineral { def create(t:Tile, bg:Tile, c:Color) = Mineral(t, bg, c, 3) }
case class Mineral(override val tile:Tile, override val bg:Tile, c:Color, hp:Int) extends TileType {
  val toolDamage = 1
  override val tileClass = TileClass.Rock
  def setHp(i:Int) = copy(hp=i)
  override def hit = { case (ij, dmg) =>
    import Profundus._
    val newHp =  hp.drop(dmg)
    val newTile = if (newHp > 0) {
      copy(hp=newHp)
    } else {
      Empty(bg, true)
    }
    (newTile, Seq(Seq(MineralPickup.create(ij, c).toPickup).ws), toolDamage, newHp <= 0)
  }
}

object Spike { def create(t:Tile, bg:Tile, spike:Direction) = Spike(t, bg, spike, 0) }
case class Spike(override val tile:Tile, override val bg:Tile, spike:Direction, hp:Int) extends TileType {
  val toolDamage = 1
  override val tileClass = TileClass.Rock
  def setHp(i:Int) = copy(hp=i)
  override def hit = toEmpty(toolDamage, hp, setHp)
}

case class WorldTile(state:TileType) {
  def isWalkable = state.isWalkable
  def tile = state.tile
  def canBreakBy(s:Seq[TileClass]) = s.contains(state.tileClass)
  def draw(ij:Cell)(tr:TileRenderer):TileRenderer = {
    val tile = state.tile
    tr <+ (ij, tile)
  }
}
