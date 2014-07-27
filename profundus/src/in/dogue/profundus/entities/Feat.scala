package in.dogue.profundus.entities

import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.antiqua.graphics.{TextFactory, TileRenderer, Tile}
import in.dogue.antiqua.data.CP437
import com.deweyvm.gleany.graphics.Color

object FeatType {
  case object Meditation extends FeatType {
    def toFeat = Feat.meditation(icon)
    def name = "Meditation"
    val icon = CP437.O.mkTile(Color.White, Color.Black)
  }
  case object Fury extends FeatType {
    def toFeat = Feat.fury(icon)
    def name = "Fury"
    val icon = CP437.`»`.mkTile(Color.Black, Color.Red)
  }
  case object Brace extends FeatType {
    def toFeat = Feat.superbrace(icon)
    def name = "Brace"
    val icon = CP437.Ω.mkTile(Color.Black, Color.Pink)
  }
  case object Repair extends FeatType {
    def toFeat = Feat.repair(icon)
    def name = "Repair"
    val icon = CP437.X.mkTile(Color.Black, Color.Green)
  }
  case object Adrenaline extends FeatType {
    def toFeat = Feat.adrenaline(icon)
    def name = "Adrenaline"
    val icon = CP437.∞.mkTile(Color.Black, Color.Brown.dim(2))
  }
  case object Garlic extends FeatType {
    def toFeat = Feat.garlic(icon)
    def name = "Garlic"
    val icon = CP437.♠.mkTile(Color.Black, Color.Tan)
  }
}

sealed trait FeatType {
  def toFeat:Feat
  def name:String
  val icon:Tile
  def makeIcon(tf:TextFactory):TileGroup = {
    Seq(((0,0), icon))
  }

  def makeDesc(tf:TextFactory):TileGroup = {
    tf.create(name).toTileGroup |++| ((-4, -3))
  }
}


object Feat {

  def NeverActivate(p:Player) = false
  def AlwaysActivate(p:Player) = true
  def MidairActivate(p:Player) = {
    p.fall != Grounded && p.fall != Floating
  }

  def DefaultOnActivate = id[Player] _
  def ToolRepairOnActivate(p:Player) = {
    p.removeFeat.repairTool
  }

  def DefaultFall(pl:Player, s:FallState) = {
    Player.setFallState(pl, s)
  }

  def BraceFall(pl:Player, s:FallState) = {
    (s, pl.fall) match {
      case (Grounded, Falling(_, _)) if pl.feat.isActivated => pl.copy(fall=s).removeFeat
      case a => DefaultFall(pl, s)
    }
  }

  def NoAllowMove(p:Player, b:Boolean) = !b
  def DefaultAllowMove(p:Player, b:Boolean) = true

  def DefaultSeekPlayer(p:Player):Option[Cell] = p.pos.some
  def NoSeekPlayer(p:Player):Option[Cell] = None

  def DefaultDoDamage(dmg:Int) = dmg
  def AdrenalineDoDamage(dmg:Int) = dmg*10
  def DefaultTakeDamage(dmg:Int) = dmg
  def AdrenalineTakeDamage(dmg:Int) = dmg/10

  def DefaultTool(p:Player) = {
    Seq(p.pos --> p.face)
  }
  def FuryTool(p:Player) = {
    Seq(p.pos --> p.face,
        p.pos --> p.face --> p.face,
        p.pos --> p.face --> p.face --> p.face,
        p.pos --> p.face --> p.face --> p.face --> p.face)
  }

  def default:Feat = {
    blank.setMayActivate(NeverActivate)
  }

  def blank:Feat = {
    Feat(None, 0, 0, false,
      DefaultAllowMove, DefaultTool, DefaultFall, DefaultDoDamage,
      DefaultTakeDamage, DefaultSeekPlayer, (0,0), DefaultOnActivate, AlwaysActivate)
  }

  def fury(icon:Tile) = {
    blank.setTime(180)
         .setTool(FuryTool)
         .setIcon(icon)
  }

  def superbrace(icon:Tile) = {
    blank.setTime(Int.MaxValue)
         .setFallFunc(BraceFall)
         .setMayActivate(MidairActivate)
         .setIcon(icon)
  }

  def repair(icon:Tile) = {
    blank.setTime(1)
         .setOnActivate(ToolRepairOnActivate)
         .setIcon(icon)
  }

  def adrenaline(icon:Tile) = {
    blank.setTime(180)
         .setDoDamage(AdrenalineDoDamage)
         .setTakeDamage(AdrenalineTakeDamage)
         .setIcon(icon)
  }

  def meditation(icon:Tile) = {
    blank.setTime(180)
         .setAllowMove(NoAllowMove)
         .setSeekPlayer(NoSeekPlayer)
         .setRestore((1,80))
         .setIcon(icon)
  }

  def garlic(icon:Tile) = {

    blank.setTime(600)
         .setSeekPlayer(NoSeekPlayer)
         .setIcon(icon)
  }
}
case class Feat(private val icon:Option[Tile], t:Int, tActivate:Int, isActivated:Boolean,
                allowMove:(Player,Boolean)=>Boolean,
                tool:Player => Seq[Cell],
                setFall: (Player, FallState) => Player,
                doDamage: Int => Int,
                takeDamage: Int => Int,
                seekPlayer: Player => Option[Cell],
                restoreAmount:(Int,Int),
                onActivate:(Player => Player),
                mayActivate:Player => Boolean) {
  def setIcon(icon:Tile) = copy(icon=icon.some)
  def setTime(t:Int) = copy(t=t)
  def setAllowMove(am:(Player,Boolean)=>Boolean) = copy(allowMove=am)
  def setTool(tool:Player => Seq[Cell]) = copy(tool=tool)
  def setFallFunc(fall:(Player, FallState) => Player) = copy(setFall=fall)
  def setDoDamage(dmg:Int=>Int) = copy(doDamage=dmg)
  def setTakeDamage(dmg:Int=>Int) = copy(takeDamage=dmg)
  def setSeekPlayer(seek:Player => Option[Cell]) = copy(seekPlayer=seek)
  def setRestore(amt:(Int,Int)) = copy(restoreAmount=amt)
  def setOnActivate(act:Player=>Player) = copy(onActivate=act)
  def setMayActivate(act:Player=>Boolean) = copy(mayActivate=act)

  def shovelPos(p:Player):Seq[Cell] = {
    val f: Player => Seq[(Int, Int)] = isActivated.select(Feat.DefaultTool, tool)
    f(p)
  }
  def tryActivate(p:Player): (Feat, Player) = {
    if (mayActivate(p)) {
      copy(tActivate = 60, isActivated = true) @@ onActivate(p)
    } else {
      this @@ p
    }
  }

  def update = {
    val newT = isActivated.select(t, t.drop1)
    if (newT <= 0 && isActivated) {
      Feat.default
    } else {
      copy(t=newT, tActivate=tActivate.drop1)
    }
  }

  def allowPlayerMove(p:Player) = {
    allowMove(p, isActivated)
  }

  def healthRestore(f:Int => HealthBar) = {
    val (amt, duration) = restoreAmount
    if (t > duration && isActivated) {
      f(amt)
    } else {
      f(0)
    }
  }

  def getPlayerPosition(pl:Player) = {
    val f:Player => Option[Cell] = isActivated.select(Feat.DefaultSeekPlayer, seekPlayer)
    f(pl)
  }

  def setFallState(pl:Player)(state:FallState):Player = {
    setFall(pl, state)
  }

  def multiplyDamage: (Int) => Int = {
    isActivated.select(Feat.DefaultDoDamage, doDamage)
  }

  def reduceDamage: (Int) => Int = {
    isActivated.select(Feat.DefaultTakeDamage, takeDamage)
  }

  def getIcon = isActivated.select(icon, None)

  def draw(xy:Cell)(tr:TileRenderer):TileRenderer = {
    if (isActivated) {
      val bound = 5
      val x = xy.x
      val y = xy.y
      val draws = for (i <- (x - bound) until (x + bound);
                       j <- (y - bound) until (y + bound)) yield {
        val ij = (i, j)
        val h = (xy |-| ij).mag
        val f = if (h < bound) {
          (t:Tile) => t.setFg(Color.White)
        } else {
          id[Tile] _
        }
        (ij,  f)
      }
      tr `$$>` draws
    } else {
      tr
    }
  }
}
