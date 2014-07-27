package in.dogue.profundus.mode.loadout

import in.dogue.profundus.Profundus
import in.dogue.profundus.entities._
import in.dogue.antiqua.graphics.{TileRenderer, Tile, TextFactory}
import in.dogue.antiqua.Antiqua._
import in.dogue.profundus.ui.{LoadoutButton, HudTool, Hud, Slider}
import in.dogue.antiqua.data.CP437
import in.dogue.profundus.ui.LoadoutButton

object Loadout {
  val tf = Profundus.tf
  val bombCost = 25
  val ropeCost = 25
  val toolCost = 32
  val diffCost = 10
  val gemCost = 1
  val fuelCost = 2
  val featCost = 0
  val default = Loadout(20,3,3,0,Shovel, FeatType.Meditation, "Debug")
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

  private def indexToFeat(i:Int):FeatType = i match {
    case 0 => FeatType.Meditation
    case 1 => FeatType.Fury
    case 2 => FeatType.Brace
    case 3 => FeatType.Repair
    case 4 => FeatType.Adrenaline
    case _ => FeatType.Garlic
  }

  private def featToIndex(f:FeatType):Int = f match {
    case FeatType.Meditation => 0
    case FeatType.Fury => 1
    case FeatType.Brace => 2
    case FeatType.Repair  => 3
    case FeatType.Adrenaline => 4
    case _/*FeatType.Garlic*/ => 5
  }

  def drawNumber(tf:TextFactory)(v:Int):TileGroup = {
    tf.create("%3s".format(v.toString)).filterToTileGroup(CP437.notBlank)
  }
  def drawTool(v:Int) = {
    indexToTool(v).icon
  }

  def drawFeat(v:Int) = {
    indexToFeat(v).makeIcon(Profundus.tf)
  }

  def descFeat(v:Int) = {
    indexToFeat(v).makeDesc(Profundus.tf)
  }

  def fillBombs(v:Int)(lo:Loadout) = lo.copy(capsules = v)
  def fillRopes(v:Int)(lo:Loadout) = lo.copy(ropes = v)
  def fillGems(v:Int)(lo:Loadout) = lo.copy(minerals = v)
  def fillFuel(v:Int)(lo:Loadout) = lo.copy(fuel = v)
  def fillTool(v:Int)(lo:Loadout) = {
    val tool = indexToTool(v)
    lo.copy(tool=tool)
  }
  def fillFeat(v:Int)(lo:Loadout) = {
    val feat = indexToFeat(v)
    lo.copy(feat=feat)
  }

  def defaultDraw(tg:TileGroup)(tr:TileRenderer):TileRenderer = {
    tr <|| tg
  }

  def fullDraw(tg:TileGroup)(tr:TileRenderer):TileRenderer = {
    tr <++ tg
  }

  def makeSimpleSlider(ij:Cell, icon:Tile, fillIn:Int => Loadout => Loadout, cost:Int, incr:Int)(value:Int) = {
    val slider = Slider.create(ij, Seq(((0,0),icon)), _ => Seq(), Loadout.drawNumber(tf), defaultDraw, fillIn, _ => 3, Int.MaxValue, value, cost, incr).toLoadoutButton
    val newRem = value*cost
    (newRem, slider)
  }

  def makeSliders(rem:Int, lo:Loadout):(Int, Vector[LoadoutButton[Slider]]) = {
    val x0 = 4
    val x1 = x0 + 6
    val y0 = 11 + LoadoutMode.topp
    val y1 = y0 + 5
    val (r1, cap) =  makeSimpleSlider((x0, y0), Capsule.stick, fillBombs, bombCost, 1)(lo.capsules)
    val (r2, rope) = makeSimpleSlider((x1, y0), Hud.ropeIcon, fillRopes, ropeCost, 1)(lo.ropes)
    val (r3, gem) =  makeSimpleSlider((x0, y1), Hud.gemIcon, fillGems, gemCost, 5)(lo.minerals)
    val (r4, fuel) = makeSimpleSlider((x1, y1), Hud.fuelIcon, fillFuel, fuelCost, 1)(lo.fuel)
    (rem - (r1 + r2 + r3 + r4), Vector(cap, rope, gem, fuel))
  }

  def makeTool(rem:Int, lo:Loadout):(Int, LoadoutButton[Slider]) = {
    val v = toolToIndex(lo.tool)
    val minus = v*toolCost
    val s = Slider.create((22, 8 + LoadoutMode.topp), Seq(), _ => Seq(), drawTool, defaultDraw, fillTool, _ => 4, 3, v, toolCost, 1).toLoadoutButton
    (rem - minus, s)
  }

  def makeFeat(rem:Int, lo:Loadout):(Int, LoadoutButton[Slider]) = {
    val v = featToIndex(lo.feat)
    val minus = v * featCost
    val s = Slider.create((23, 18 + LoadoutMode.topp), Seq(), descFeat, drawFeat, fullDraw, fillFeat, _ => 1, 5, v, featCost, 1).toLoadoutButton
    (rem - minus, s)
  }
}

case class Loadout(fuel:Int, ropes:Int, capsules:Int, minerals:Int, tool:ToolType, feat:FeatType, name:String)
