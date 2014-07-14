package in.dogue.profundus.mode

import in.dogue.antiqua.graphics._
import in.dogue.profundus.input.Controls
import in.dogue.antiqua.data.CP437
import in.dogue.antiqua.Antiqua
import Antiqua._
import com.deweyvm.gleany.graphics.Color
import in.dogue.profundus.ui.{Slider, Hud}
import in.dogue.profundus.entities._
import scala.util.Random
import in.dogue.antiqua.graphics.Text
import in.dogue.profundus.world.{Difficulty, Easy, Normal, Hard}

object LoadoutMode {
  def create(cols:Int, rows:Int, initial:Option[Loadout]) = {
    val pts = 210
    def mk(r:Random) = {
      import Color._
      val bg = Grey.dim(6 + r.nextDouble)
      val fg = Grey.dim(3 + r.nextDouble)
      val code = Vector(CP437.`-`, CP437.`=`, CP437.`≡`, CP437.` `).randomR(r)
      code.mkTile(bg, fg)
    }
    val rect = Rect.createTextured(cols, rows, mk, new Random(0))
    val border = Border.standard(CP437.doubleBorder, Color.Black, Color.White)(cols, rows)
    val tf = TextFactory(Color.Black, Color.White, CP437.unicodeToCode)
    val lo = initial.getOrElse(Loadout.default)
    val (rem, std) = Loadout.makeSliders(pts, lo)
    val (rem2, tool) = Loadout.makeTool(rem, lo)
    val (rem3, diff) = Loadout.makeDiff(rem2, lo)
    val ptText = tf.create("Remaining: ")
    val sliders =  std ++ Vector(tool, diff)
    val top =   LoadoutSection.create(     cols,             5,         0,      0, "", tf)
    val left =  LoadoutSection.create(cols - 14,            19,         0,      5, "Provisions", tf)
    val right = LoadoutSection.create(       14,            19, cols - 14,      5, "Tools", tf)
    val bott  = LoadoutSection.create(     cols, rows - 19 - 5,         0, 5 + 19, "Cavern Severity", tf)
    val secs = Vector(top, left, right, bott)
    LoadoutMode(cols, rows, tf, sliders, 0, ptText, rem3, rect, border, secs)
  }
}
object LoadoutSection {
  def create(cols:Int, rows:Int, i:Int, j:Int, title:String, tr:TextFactory) = {
    val border = Border.standard(CP437.doubleBorder, Color.Black, Color.White)(cols, rows)
    val text = tr.create(title)
    LoadoutSection(i, j, border, text)
  }
}

case class LoadoutSection private (i:Int, j:Int, b:Border, text:Text) {
  def draw(tr:TileRenderer):TileRenderer = {
    tr <+< b.draw(i, j) <+< text.draw(i+1, j+1)
  }
}

object Loadout {
  val tf = TextFactory(Color.Black, Color.White, CP437.unicodeToCode)
  val bombCost = 25
  val ropeCost = 25
  val toolCost = 32
  val diffCost = 10
  val gemCost = 1
  val fuelCost = 2
  val default = Loadout(0,0,0,0,Shovel,Normal)
  private def indexToTool(v:Int) = v match {
    case 0 => Shovel
    case 1 => Mallet
    case 2 => Mattock
    case 3 => Rapier
  }

  private def toolToIndex(t:ToolType):Int = t match {
    case Shovel => 0
    case Mallet => 1
    case Mattock => 2
    case Rapier => 3
  }

  private def indexToDiff(v:Int) = v match {
    case 2 => Easy
    case 1 => Normal
    case 0 => Hard
  }

  private def diffToIndex(v:Difficulty) = v match {
    case Easy => 2
    case Normal => 1
    case Hard => 0
  }

  def drawNumber(tf:TextFactory)(v:Int):TileGroup = {
    tf.create("%3s".format(v.toString)).toTileGroup
  }
  def drawTool(v:Int) = {
    indexToTool(v).icon
  }
  def drawDiff(tf:TextFactory)(v:Int):TileGroup = {
    val s = indexToDiff(v).name
    tf.create(s).toTileGroup
  }

  def fillBombs(v:Int)(lo:Loadout) = lo.copy(bombs = v)
  def fillRopes(v:Int)(lo:Loadout) = lo.copy(ropes = v)
  def fillGems(v:Int)(lo:Loadout) = lo.copy(gems = v)
  def fillFuel(v:Int)(lo:Loadout) = lo.copy(fuel = v)
  def fillDiff(v:Int)(lo:Loadout) = {
    val diff = indexToDiff(v)
    lo.copy(diff = diff)
  }
  def fillTool(v:Int)(lo:Loadout) = {
    val tool = indexToTool(v)
    lo.copy(`type`=tool)
  }

  def makeSimpleSlider(i:Int, j:Int, icon:Tile, fillIn:Int => Loadout => Loadout, cost:Int, incr:Int)(value:Int) = {
    val slider = Slider.create(i, j, Seq((0,0,icon)), Loadout.drawNumber(tf), fillIn, Int.MaxValue, value, cost, incr)
    val newRem = value*cost
    (newRem, slider)
  }

  def makeSliders(rem:Int, lo:Loadout):(Int, Vector[Slider]) = {
    val x0 = 4
    val x1 = x0 + 6
    val y0 = 12
    val y1 = y0 + 5
    val (r1, cap) =  makeSimpleSlider(x0, y0, Capsule.stick, fillBombs, bombCost, 1)(lo.bombs)
    val (r2, rope) = makeSimpleSlider(x1, y0, Hud.ropeIcon, fillRopes, ropeCost, 1)(lo.ropes)
    val (r3, gem) =  makeSimpleSlider(x0, y1, Hud.gemIcon, fillGems, gemCost*5, 5)(lo.gems)
    val (r4, fuel) = makeSimpleSlider(x1, y1, Hud.fuelIcon, fillFuel, fuelCost, 1)(lo.fuel)
    (rem - (r1 + r2 + r3 + r4), Vector(cap, gem, rope, fuel))
  }

  def makeTool(rem:Int, lo:Loadout):(Int, Slider) = {
    val v = toolToIndex(lo.`type`)
    val minus = v*toolCost
    val s = Slider.create(22, 14, Seq(), drawTool, fillTool, 3, 0, toolCost, 1)
    (rem - minus, s)
  }

  def makeDiff(rem:Int, lo:Loadout):(Int, Slider) = {
    val v = diffToIndex(lo.diff)
    val minus = v*diffCost
    val s = Slider.create(11, 28, Seq(), drawDiff(tf), fillDiff, 2, 1, diffCost, 1)
    (rem - minus, s)
  }
}

case class Loadout(fuel:Int, ropes:Int, bombs:Int, gems:Int, `type`:ToolType, diff:Difficulty)

case class LoadoutMode private (cols:Int, rows:Int, tf:TextFactory, sliders:IndexedSeq[Slider], ptr:Int, ptText:Text, points:Int, r:Rect, b:Border, los:Seq[LoadoutSection]) {

  private def move:LoadoutMode = {
    Controls.AxisX.zip(15,5) match {
      case 1 if ptr < sliders.length - 1 => copy(ptr=ptr+1)
      case -1 if ptr > 0 => copy(ptr = ptr.drop1)
      case _ => this
    }
  }
  def update = {

    if (Controls.Space.justPressed) {
      CircleTransition.create(cols, rows, this.toMode, GameMode.create(cols, rows, getLoadout).toMode).toMode
    } else {
      val (newS, newP) = sliders(ptr).update(points)
      val newSliders = sliders.updated(ptr, newS)
      copy(sliders=newSliders, points=newP).move.toMode
    }
  }

  private def getLoadout = {
    sliders.foldLeft(Loadout.default) { case (lo, sl) =>
      sl.doFill(lo)
    }
  }

  def draw(tr:TileRenderer):TileRenderer = {
    val ptX = 8
    val ptY = 2
    (tr <+< r.draw(0,0)
        <++< sliders.zipWithIndex.map { case (s, i) => s.draw(i == ptr, points) _}
        <+< ptText.draw(ptX, ptY)
        <+< tf.create("%3s☼".format(points.toString)).draw(ptX + ptText.length, ptY)
        <+< b.draw(0,0)
        <++< los.map {_.draw _}
      )
  }
  def toMode:Mode[LoadoutMode] = Mode(_.update, _.draw, this)
}
