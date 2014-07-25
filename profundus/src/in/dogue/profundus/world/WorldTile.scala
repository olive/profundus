package in.dogue.profundus.world

import in.dogue.antiqua.graphics.{TileRenderer, Tile}
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.antiqua.data.Direction
import in.dogue.profundus.entities.pickups.{Pickup, MineralPickup}
import in.dogue.profundus.entities.Damage
import scala.util.Random
import in.dogue.profundus.Profundus
import Profundus._
import scala.collection.immutable.Stream

object TileClass {
  case object Dirt extends TileClass
  case object Clay extends TileClass
  case object Rock1 extends TileClass
  case object Rock2 extends TileClass
  case object Rock3 extends TileClass
  case object Invincible extends TileClass
}
sealed trait TileClass


/*

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

object Rock3 { def create(t:Tile, bg:Tile) = Rock3(t, bg, 50, Seq(), Seq()) }
case class Rock3(override val tile:Tile,
                 override val bg:Tile,
                 hp:Int,
                 override val onKill:Seq[WorldSpawn],
                 override val dependents:Seq[Cell]) extends TileType {
  def append(s:Seq[WorldSpawn]) = copy(onKill=onKill++s)
  val toolDamage = 50
  override val tileClass = TileClass.Rock
  def setHp(i:Int) = copy(hp=i)
  override def hit = toEmpty(toolDamage, hp, setHp)
}

object Rock2 { def create(t:Tile, bg:Tile) = Rock2(t, bg, 15, Seq(), Seq()) }
case class Rock2(override val tile:Tile,
                 override val bg:Tile,
                 hp:Int,
                 override val onKill:Seq[WorldSpawn],
                 override val dependents:Seq[Cell]) extends TileType {
  def append(s:Seq[WorldSpawn]) = copy(onKill=onKill++s)
  val toolDamage = 15
  override val tileClass = TileClass.Rock
  def setHp(i:Int) = copy(hp=i)
  override def hit = toEmpty(toolDamage, hp, setHp)
}

object Rock { def create(t:Tile, bg:Tile) = Rock(t, bg, 5, Seq(), Seq()) }
case class Rock(override val tile:Tile,
                override val bg:Tile,
                hp:Int,
                override val onKill:Seq[WorldSpawn],
                override val dependents:Seq[Cell]) extends TileType {
  def append(s:Seq[WorldSpawn]) = copy(onKill=onKill++s)
  val toolDamage = 5
  override val tileClass = TileClass.Rock
  def setHp(i:Int) = copy(hp=i)
  override def hit = toEmpty(toolDamage, hp, setHp)
}

object Clay { def create(t:Tile, bg:Tile) = Clay(t, bg, 2, Seq(), Seq()) }
case class Clay(override val tile:Tile,
                override val bg:Tile,
                hp:Int,
                override val onKill:Seq[WorldSpawn],
                override val dependents:Seq[Cell]) extends TileType {
  def append(s:Seq[WorldSpawn]) = copy(onKill=onKill++s)
  val toolDamage = 5
  override val tileClass = TileClass.Clay
  def setHp(i:Int) = copy(hp=i)
  override def hit = toEmpty(toolDamage, hp, setHp)
}

object Dirt { def create(t:Tile, bg:Tile) = Dirt(t, bg, 1, Seq(), Seq()) }
case class Dirt(override val tile:Tile,
                override val bg:Tile,
                hp:Int,
                override val onKill:Seq[WorldSpawn],
                override val dependents:Seq[Cell]) extends TileType {
  def append(s:Seq[WorldSpawn]) = copy(onKill=onKill++s)
  val toolDamage = 1
  override val tileClass = TileClass.Dirt
  def setHp(i:Int) = copy(hp=i)
  override def hit = toEmpty(toolDamage, hp, setHp)
}

object Mineral { def create(t:Tile, bg:Tile, c:Color) = Mineral(t, bg, c, 3, Seq(), Seq()) }
case class Mineral(override val tile:Tile,
                   override val bg:Tile,
                   c:Color,
                   hp:Int,
                   override val onKill:Seq[WorldSpawn],
                   override val dependents:Seq[Cell]) extends TileType {
  def append(s:Seq[WorldSpawn]) = copy(onKill=onKill++s)
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
    (newTile, Seq(MineralPickup.create(ij, c).toPickup).gss, toolDamage, newHp <= 0)
  }
}

object Spike { def create(t:Tile, bg:Tile, spike:Direction) = Spike(t, bg, spike, 0) }
case class Spike(override val tile:Tile,
                 override val bg:Tile,
                 spike:Direction,
                 hp:Int) extends TileType {
  val toolDamage = 1
  override val tileClass = TileClass.Rock
  def setHp(i:Int) = copy(hp=i)
  override def hit = toEmpty(toolDamage, hp, setHp)
}
sealed trait TileType {
  val onKill:Seq[WorldSpawn] = Seq()
  val dependents:Seq[Cell] = Seq()
  val tile:Tile
  val tileClass:TileClass
  val bg:Tile
  val isWalkable:Boolean = false
  type Update = (TileType, Seq[WorldSpawn], Int, Boolean)
  def hit:(Cell, Int) => Update
  def standard(f:(Cell, Int) => (TileType, Int, Boolean)):(Cell, Int) => Update = { case (ij, dmg) =>
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

*/
sealed trait TileType
case class Empty(bgSolid:Boolean) extends TileType
case class Spike(d:Direction) extends TileType
case object Dirt extends TileType
case object Clay extends TileType
case object Rock1 extends TileType
case object Rock2 extends TileType
case object Rock3 extends TileType
case object Shaft extends TileType

trait WorldTileFactory {
  val ts:TerrainScheme
  val r:Random
  type MkTile = (WorldTile, Option[Cell])
  private def mkSimple(tile:Tile, ttype:TileType, tclass:TileClass, hp:Int, toolDmg:Int):MkTile = {
    WorldTile(tile, ttype, tclass, hp, toolDmg, 0, _ => Seq(), Seq()) @@ None
  }
  def mkEmpty:MkTile = mkSimple(ts.empty(r), Empty(true), TileClass.Invincible, 1, 0)
  def mkDirt:MkTile =  mkSimple(ts.dirt(r),  Dirt,  TileClass.Dirt, 1, 1)
  def mkClay:MkTile =  mkSimple(ts.clay(r),  Clay,  TileClass.Clay, 2, 5)
  def mkRock1:MkTile = mkSimple(ts.rock1(r), Rock1, TileClass.Rock1, 10, 5)
  def mkRock2:MkTile = mkSimple(ts.rock2(r), Rock2, TileClass.Rock2, 20, 20)
  def mkRock3:MkTile = mkSimple(ts.rock3(r), Rock3, TileClass.Rock3, 50, 50)
  def mkShaft:MkTile = mkSimple(ts.shaft(r), Shaft, TileClass.Invincible, 1, 0)
  def mkMineral:MkTile = {
    val (tile, color) = ts.mineral(r)
    val spawn = (ij:Cell) => Seq(MineralPickup.create(ij, color).toPickup).gss
    WorldTile(tile, Rock1, TileClass.Rock1, 10, 5, 0, spawn, Seq()) @@ None
  }
  def mkSpike(ij:Cell, d:Direction) = {
    val sp = Spike(d)
    val basis = ij --> d.opposite
    WorldTile(ts.spike(d)(r), sp, TileClass.Rock1, 15, 5, 0 , _ => Seq(), Seq()) @@ basis.some
  }
}

case class HitResult(broken:Boolean, toolHurt:Int, healthHurt:Int)
case class WorldTile(tile:Tile, ttype:TileType, tclass:TileClass, hp:Int, toolDamage:Int, healthDamage:Int, onKill:Cell => Seq[WorldSpawn], dependents:Seq[Cell]) {

  def addDep(ij:Cell) = {
    copy(dependents = ij +: dependents)
  }
  /**
   * Notify this tile that a tile it is dependent on has been destroyed
   * @return the modified version of 'this'
   */
  def notifyTile(tf:WorldTileFactory, ij:Cell):(WorldTile, Seq[WorldSpawn]) = {
    (tf.mkEmpty._1, Seq())
  }
  def hit(tf:WorldTileFactory, ij:Cell, dmg:Damage):(WorldTile, Seq[WorldSpawn], HitResult) = {
    val amt = dmg.amount
    val newHp = hp - amt
    if (newHp <= 0) {
      tf.mkEmpty._1 @@ onKill(ij) @@ HitResult(true, toolDamage, healthDamage)
    } else {
      copy(hp=newHp) @@ Seq() @@ HitResult(false, toolDamage, healthDamage)
    }
  }

  def canBreakBy(s:Seq[TileClass]) = s.contains(tclass)
  def isWalkable = ttype match {
    case Empty(_) => true
    case _ => false
  }
  def isBgSolid = ttype match {
    case Empty(true) => true
    case _ => false
  }
  def isRock = ttype match {
    case Rock1 => true
    case Rock2 => true
    case Rock3 => true
    case _ => false
  }

  def draw(ij:Cell)(tr:TileRenderer):TileRenderer = {
    tr <+ (ij, tile)
  }

}
/*case class WorldTile(state:TileType) {
  def isWalkable = state.isWalkable
  def isRock = state match {
    case r@Rock(_,_,_,_,_) => true
    case r@Rock2(_,_,_,_,_) => true
    case r@Rock3(_,_,_,_,_) => true
    case _ => false
  }
  def tile = state.tile
  def canBreakBy(s:Seq[TileClass]) = s.contains(state.tileClass)
  def draw(ij:Cell)(tr:TileRenderer):TileRenderer = {
    val tile = state.tile
    tr <+ (ij, tile)
  }
}*/
