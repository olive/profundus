package in.dogue.profundus.entities

import in.dogue.profundus.world.{WorldSpawn, TerrainCache}
import scala.util.Random
import in.dogue.antiqua.graphics.{TileRenderer, Tile}
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.profundus.lighting.LightSource
import in.dogue.profundus.ui.{MessageBoxComplete, MessageBoxContinue, MessageBox}
import in.dogue.profundus.Profundus
import in.dogue.antiqua.data.{Direction, CP437}
import com.deweyvm.gleany.graphics.Color
import in.dogue.profundus.input.Controls
import in.dogue.profundus.particles.LaserBoom
import in.dogue.profundus.audio.SoundManager
import in.dogue.profundus.entities.damagezones.DelayedExplosion

object Phoebe {
  def create(ij:Cell, tbs:Vector[Vector[String]], r:Random) = {
    val light = LightSource.createCircle(ij, 0, 25, 0.5)
    val mbox = tbs.map{tb => MessageBox.create(Profundus.tf, tb, () => (), Seq()) }
    val tile = CP437.P.mkTile(Color.Black, Color.White)
    val arrow = CP437.↑.mkTile(Color.Black, Color.White)
    val hp = 100
    val npc = Phoebe(tile, arrow, mbox, 0, false, false, hp, 0)
    StandardEntity.create[Phoebe](_.update, _.draw, npc, light, true, DamageType.Phoebe.some, hp, r).toEntity(ij)
  }
}


case class Phoebe(a:Tile, arrow:Tile, boxes:Vector[MessageBox[Unit]], ptr:Int, close:Boolean, aggroed:Boolean, maxHealth:Int, t:Int) {

  private def getBox = boxes(ptr)
  def update(health:Int, t:Int, pos:Cell, cache:TerrainCache, pi:PlayerInfo, r:Random): (Phoebe, Cell, Seq[WorldSpawn]) = {
    import Profundus._
    val ppos = pi.pos
    val isClose = (pos |-| ppos).mag < 5 && !aggroed
    val showMb = Controls.Up.justPressed && isClose
    val (spawn, newPtr) = if (showMb) {
      val box = GameBox(pos |+| ((-19,10)), getBox).gss
      (box, (ptr+1) % boxes.length)
    } else {
      (Seq(), ptr)
    }

    val newPos = if (aggroed) {
      val dd = ppos |-| pos
      val onTop = dd.mag == 0
      val adjacent = dd.x.abs + dd.y.abs == 1
      val dx = if (onTop) {
        -1
      } else if (!adjacent){
        dd.x.signum
      } else {
        0
      }
      pos |+ dx
    } else {
      pos
    }


    val attacks = if (aggroed && t % 5 == 0) {
      SoundManager.pew.play(pos)
      val x = r.nextInt(30) - 15
      val y = r.nextInt(30) - 15
      val dst = (x, y) |+| pos
      val boom = LaserBoom.create(pos, dst, 3)
      val hurt  = DelayedExplosion(dst, boom.timeToExplode, 30, 10, 0)
      Seq(Seq(boom.toParticle).gs, Seq(hurt.toZone).gs)
    } else {
      Seq()
    }
    val newSelf = copy(t=t+1, close=isClose, ptr = newPtr, aggroed = health<maxHealth)
    newSelf @@ newPos @@ (spawn ++ attacks)
  }

  def draw(ij:Cell)(tr:TileRenderer):TileRenderer = {
    tr <+ (ij, a) <+? (ij -| 2, arrow).onlyIf(close && t % 30 < 15)
  }
}
