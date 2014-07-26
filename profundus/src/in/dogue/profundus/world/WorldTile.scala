package in.dogue.profundus.world

import in.dogue.antiqua.graphics.{TileRenderer, Tile}
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.antiqua.data.{CP437, Direction}
import in.dogue.profundus.entities.pickups.MineralPickup
import in.dogue.profundus.entities.{Stalactite, Damage}
import scala.util.Random
import in.dogue.profundus.Profundus
import Profundus._

object TileClass {
  case object Dirt extends TileClass
  case object Clay extends TileClass
  case object Rock1 extends TileClass
  case object Rock2 extends TileClass
  case object Rock3 extends TileClass
  case object Invincible extends TileClass
}
sealed trait TileClass

sealed trait TileType
case class Empty(bgSolid:Boolean) extends TileType
case class Spike(d:Direction) extends TileType
case object Dirt extends TileType
case object Clay extends TileType
case object Rock1 extends TileType
case object Rock2 extends TileType
case object Rock3 extends TileType
case object Mineral extends TileType
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
    WorldTile(tile, Mineral, TileClass.Rock1, 10, 5, 0, spawn, Seq()) @@ None
  }
  def mkSpike(ij:Cell, d:Direction) = {
    val sp = Spike(d)
    val basis = ij --> d.opposite
    WorldTile(ts.spike(d)(r), sp, TileClass.Rock1, 15, 5, 0 , _ => Seq(), Seq()) @@ basis.some
  }
}

case class HitResult(broken:Boolean, toolHurt:Int, healthHurt:Int)
case class WorldTile(tile:Tile, ttype:TileType, tclass:TileClass, hp:Int, toolDamage:Int, healthDamage:Int, onKill:Cell => Seq[GlobalMessage], dependents:Seq[Cell]) {

  def addDep(ij:Cell) = {
    copy(dependents = ij +: dependents)
  }
  /**
   * Notify this tile that a tile it is dependent on has been destroyed
   * @return the modified version of 'this'
   */
  def notifyTile(tf:WorldTileFactory, ij:Cell, y:Int):(WorldTile, Seq[GlobalMessage]) = {
    (tf.mkEmpty._1, Seq(Stalactite.create(ij +| y, copy(tile.setCode(CP437.▼)))).gss)
  }
  def hit(tf:WorldTileFactory, ij:Cell, dmg:Damage):(WorldTile, Seq[GlobalMessage], HitResult) = {
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

