package in.dogue.profundus.entities.pickups

import in.dogue.antiqua.data.{Code, CP437}
import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.graphics.{TileRenderer, Tile}
import in.dogue.antiqua.Antiqua.Cell
import in.dogue.profundus.entities.{Attributes, Buff, Player}
import in.dogue.antiqua.Antiqua
import Antiqua._
import scala.util.Random
import in.dogue.profundus.entities.pickups.FoodType._
import in.dogue.profundus.entities.Buff


object FoodHueGen {
  val all = Vector(VeryBad, SlightlyBad, SlightlyGood, VeryGood)
  def create(r:Random) = {
    val hue = r.nextDouble
    val sat = 0.55 + 0.2*r.nextDouble
    val lum = 0.55 + 0.2*r.nextDouble
    val hues = r.shuffle((0 until 4).map{ i => (hue + i*0.125) % 1.0}).toVector
    def mkColor(k:Int) = Color.fromHsb(hues(k), sat, lum)
    new FoodHueGen {
      override def getColor(hue:FoodHue) = hue match {
        case SlightlyBad => mkColor(0)
        case SlightlyGood => mkColor(1)
        case VeryBad => mkColor(2)
        case VeryGood => mkColor(3)
      }
    }
  }
  def randomHue(r:Random) = all.randomR(r)
}

sealed trait FoodHue
case object VeryBad extends FoodHue
case object SlightlyBad extends FoodHue
case object SlightlyGood extends FoodHue
case object VeryGood extends FoodHue

sealed trait FoodHueGen {
  def getColor(hue:FoodHue):Color
}
sealed trait FoodType
object FoodType {

  case object Toadstool extends FoodType
  case object Bacon extends FoodType
  case object Flutter extends FoodType
  case object Cloverb extends FoodType
  case object WaffleRoot extends FoodType
  case object Bloatstool extends FoodType
  case object DeathCap extends FoodType


  val All = Vector(Toadstool, Bacon, Cloverb, Flutter, WaffleRoot, Bloatstool, DeathCap)
  def getRandom(r:Random) = All.randomR(r)
}

object FoodFactory {
  def create(r:Random) = {
    val map: Map[FoodType, FoodHueGen] = FoodType.All.map{ ft => ft -> FoodHueGen.create(r) }.toMap
    FoodFactory(map, r)
  }
}
case class FoodFactory(f:Map[FoodType, FoodHueGen], r:Random) {

  def mkToadstool(r:Random) = {
    def process(hue:FoodHue)(attr:Attributes) = {
      val amt = hue match {
        case VeryBad => -2
        case SlightlyBad => -1
        case SlightlyGood => 1
        case VeryGood => 2
      }
      attr.incStam(amt)
    }
    mkSimple(Toadstool, CP437.τ, process, r)
  }

  def mkCloverb(r:Random) = {
    def process(hue:FoodHue)(attr:Attributes) = {
      val amt = hue match {
        case VeryBad => -2
        case SlightlyBad => -1
        case SlightlyGood => 1
        case VeryGood => 2
      }
      attr.incHealthRegen(amt)
    }
    mkSimple(Cloverb, CP437.♠, process, r)
  }

  def mkFlutter(r:Random) = {
    def process(hue:FoodHue)(attr:Attributes) = {
      val amt = hue match {
        case VeryBad => -2
        case SlightlyBad => -1
        case SlightlyGood => 1
        case VeryGood => 2
      }
      attr.incAirMove(amt)
    }
    mkSimple(Flutter, CP437.`»`, process, r)
  }

  def mkWaffleRoot(r:Random) = {
    def process(hue:FoodHue)(attr:Attributes) = {
      val amt = hue match {
        case VeryBad => -2
        case SlightlyBad => -1
        case SlightlyGood => 1
        case VeryGood => 2
      }
      attr.incRadius(amt)
    }
    mkSimple(WaffleRoot, CP437.`#`, process, r)
  }

  def mkBacon(r:Random) = {
    def process(hue:FoodHue)(attr:Attributes) = {
      val amt = hue match {
        case VeryBad => -10
        case SlightlyBad => -5
        case SlightlyGood => 5
        case VeryGood => 10
      }
      attr.restore(amt)
    }
    mkSimple(Bacon, CP437.`~`, process, r)
  }

  def mkBloatstool(r:Random) = {
    def process(hue:FoodHue)(attr:Attributes) = {
      val (fall, defe) = hue match {
        case VeryBad => (2, -0.1)
        case SlightlyBad => (1, -0.05)
        case SlightlyGood => (1, 0.05)
        case VeryGood => (0, 0.1)
      }
      attr.incGravity(fall).incDefense(defe)
    }
    mkSimple(Bloatstool, CP437.■, process, r)
  }

  def mkDeathCap(r:Random) = {
    def process(hue:FoodHue)(attr:Attributes) = {
      val (maxHp, defe, att, light) = hue match {
        case VeryBad => (-100, -0.3, -0.3, -3)
        case SlightlyBad => (-100, -0.1, -0.1, -1)
        case SlightlyGood => (-100, 0.0, 0.0, 0)
        case VeryGood => (50, 2.0, 2.0, 3)
      }
      attr.restore(maxHp).incDefense(defe).incAttack(att).incRadius(light)
    }
    mkSimple(Bloatstool, CP437.■, process, r)
  }

  def mkSimple(typ:FoodType, code:Code, process:FoodHue => Attributes => Attributes, r:Random) = {
    val hueGen = f(typ)
    val hue: FoodHue = FoodHueGen.randomHue(r)
    val color = hueGen.getColor(hue)
    Buff(code, color, typ, process(hue))
  }

  val All = Vector(mkCloverb _,
                   mkToadstool _,
                   mkFlutter _,
                   mkWaffleRoot _,
                   mkBacon _,
                   mkBloatstool _,
                   mkDeathCap _)

}

object FoodPickup {
  def create(ij:Cell, ft:Random => Buff, r:Random) = {
    val buff = ft(r)
    FoodPickup(ij, buff.getIcon, buff)
  }

}
case class FoodPickup private (ij:Cell, a:Tile, buff:Buff) {
  def update = this

  def isCollectable(p:Player) = true
  def onPickup(pl:Player) = {
    pl.collectFood(buff)
  }
  def draw(ij:Cell)(tr:TileRenderer):TileRenderer = {
    tr <+ (ij, a)
  }

  def toPickup:Pickup = Pickup.create[FoodPickup](ij, _.update, _.isCollectable, _.onPickup, _.draw, this)
}
