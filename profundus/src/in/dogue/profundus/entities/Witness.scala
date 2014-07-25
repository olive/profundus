package in.dogue.profundus.entities

import in.dogue.antiqua.data.{Direction, Code, CP437}
import in.dogue.antiqua.graphics.{TileRenderer, Tile, TileFactory, Animation}
import scala.util.Random
import in.dogue.antiqua.Antiqua.{AnimationGroup, Cell}
import com.deweyvm.gleany.graphics.Color
import in.dogue.profundus.world.{WorldSpawn, TerrainCache}
import in.dogue.profundus.lighting.LightSource
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.profundus.entities.damagezones.SingleTileZone
import in.dogue.profundus.audio.SoundManager
import in.dogue.profundus.Profundus

object Witness {
  def makeAnims(r:Random) = {
    val tf = TileFactory(Color.Black, Color.White)
    val badEye = TileFactory(Color.Black, Color.Red)
    def s(c:Code) = Animation.singleton(tf(c))
    def makeTentacle = {
      val v1 = r.nextInt(15) + 7
      val v2 = r.nextInt(15) + 7
      val v3 = r.nextInt(15) + 7
      Animation.create(Vector(
        (v1, tf(CP437.`)`)),
        (v2, tf(CP437.│)),
        (v3, tf(CP437.`(`))

      ))
    }
    val petrieye = Animation.create(Vector(
      (3, tf(CP437.☼)),
      (3, badEye(CP437.Θ))
    ))
    def makeBody(eye:Animation) = {
      Vector(
        ((-2,1),  s(CP437./)),
        ((-1,0),  s(CP437./)),
        ((0,0),   eye),
        ((0,-1),  s(CP437.underscore)),
        ((1, 0),  s(CP437.\)),
        ((2, 1),  s(CP437.\)),
        ((1, 1),  s(CP437.│)),
        ((-1, 1), s(CP437.│)),
        ((1, 2), makeTentacle),
        ((-1, 2), makeTentacle)
      )
    }
    makeBody(s(CP437.☼)) @@ makeBody(petrieye)


  }
  def create(ij:Cell, r:Random) = {
    val (norm, kill) = makeAnims(r)
    val w = Witness(norm, kill, 0)
    val light = LightSource.createCircle(ij, 0, 7, 1)
    StandardEntity.create[Witness](_.update, _.draw, w, light, true, DamageType.Witness.some, 50, r).toEntity(ij)
  }
}

case class Witness(normalAnim:AnimationGroup, killAnim:AnimationGroup, killT:Int) {

  private def getAnim = if (killT > 0) {
    killAnim
  } else {
    normalAnim
  }

  def update(health:Int, t:Int, pos:Cell, cache:TerrainCache, ppos:Cell, pState:LivingState, r:Random): (Witness, Cell, Seq[WorldSpawn]) = {
    import Profundus._
    val dd = ppos |-| pos
    val killer = if (dd.mag < 10 && cache.hasLineOfSight(ppos, pos)) {
      copy(killT=killT+1)
    } else {
      copy(killT=0)
    }

    val newPos = if (killT == 0 && t % 120 == 0) {
      val d = Direction.All.randomR(r)
      if (!cache.isSolid(pos --> d)) {
        pos --> d
      } else {
        pos
      }
    } else {
      pos
    }
    if (killT == 70) {
      SoundManager.petrify.playFull()
    }
    val kz = if (killT == 90) {

      val kill = SingleTileZone(ppos, 1000, DamageType.Witness).toZone
      Seq(kill).gss
    } else {
      Seq()
    }

    val newSelf = killer.copy(normalAnim.smap { _.update }, killAnim=killAnim.smap { _.update })
    newSelf @@ newPos @@ kz
  }
  def draw(ij:Cell)(tr:TileRenderer):TileRenderer = {
    tr <++< getAnim.map{ case (c, anim) => anim.drawFg(c |+| ij) _}
  }
}
