package in.dogue.profundus.mode.loadout

import in.dogue.profundus.Profundus
import in.dogue.profundus.entities._
import in.dogue.antiqua.graphics.{Tile, TextFactory}
import in.dogue.antiqua.Antiqua._
import in.dogue.profundus.ui.{LoadoutButton, HudTool, Hud, Slider}
import in.dogue.antiqua.data.CP437

object Loadout {
  val tf = Profundus.tf
  val bombCost = 25
  val ropeCost = 25
  val toolCost = 32
  val diffCost = 10
  val gemCost = 1
  val fuelCost = 2
  val default = Loadout(20,3,3,0,Shovel, "Debug")
  private def indexToTool(v:Int) = v match {
    case 0 => Shovel
    case 1 => Mallet
    case 2 => Mattock
    case 3 => Rapier
    case _ => BareHands
  }

  private def toolToIndex(t:ToolType):Int = t match {
    case Shovel => 0
    case Mallet => 1
    case Mattock => 2
    case Rapier => 3
    case _ => 4

  }

  def drawNumber(tf:TextFactory)(v:Int):TileGroup = {
    tf.create("%3s".format(v.toString)).filterToTileGroup(CP437.notBlank)
  }
  def drawTool(v:Int) = {
    indexToTool(v).icon
  }

  def fillBombs(v:Int)(lo:Loadout) = lo.copy(bombs = v)
  def fillRopes(v:Int)(lo:Loadout) = lo.copy(ropes = v)
  def fillGems(v:Int)(lo:Loadout) = lo.copy(gems = v)
  def fillFuel(v:Int)(lo:Loadout) = lo.copy(fuel = v)
  def fillTool(v:Int)(lo:Loadout) = {
    val tool = indexToTool(v)
    lo.copy(`type`=tool)
  }

  def makeSimpleSlider(i:Int, j:Int, icon:Tile, fillIn:Int => Loadout => Loadout, cost:Int, incr:Int)(value:Int) = {
    val slider = Slider.create(i, j, Seq((0,0,icon)), Loadout.drawNumber(tf), fillIn, 3, Int.MaxValue, value, cost, incr).toLoadoutButton
    val newRem = value*cost
    (newRem, slider)
  }

  def makeSliders(rem:Int, lo:Loadout):(Int, Vector[LoadoutButton[Slider]]) = {
    val x0 = 4
    val x1 = x0 + 6
    val y0 = 11 + LoadoutMode.topp
    val y1 = y0 + 5
    val (r1, cap) =  makeSimpleSlider(x0, y0, Capsule.stick, fillBombs, bombCost, 1)(lo.bombs)
    val (r2, rope) = makeSimpleSlider(x1, y0, Hud.ropeIcon, fillRopes, ropeCost, 1)(lo.ropes)
    val (r3, gem) =  makeSimpleSlider(x0, y1, Hud.gemIcon, fillGems, gemCost, 5)(lo.gems)
    val (r4, fuel) = makeSimpleSlider(x1, y1, Hud.fuelIcon, fillFuel, fuelCost, 1)(lo.fuel)
    (rem - (r1 + r2 + r3 + r4), Vector(cap, rope, gem, fuel))
  }

  def makeTool(rem:Int, lo:Loadout):(Int, LoadoutButton[Slider]) = {
    val v = toolToIndex(lo.`type`)
    val minus = v*toolCost
    val s = Slider.create(22, 13 + LoadoutMode.topp, Seq(), drawTool, fillTool, 4, 3, v, toolCost, 1).toLoadoutButton
    (rem - minus, s)
  }
}

case class Loadout(fuel:Int, ropes:Int, bombs:Int, gems:Int, `type`:ToolType, name:String)
