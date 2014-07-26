package in.dogue.profundus.entities

import in.dogue.antiqua.Antiqua._
import in.dogue.antiqua.data.{CP437, Direction}
import in.dogue.profundus.world.{GlobalMessage, TerrainCache}
import scala.util.Random
import in.dogue.profundus.entities.damagezones.{SingleTileZone, ExplosionZone}
import in.dogue.profundus.entities.pickups.ItemPickup
import in.dogue.antiqua.graphics.{Tile, TileFactory, Animation, TileRenderer}
import com.deweyvm.gleany.graphics.Color
import in.dogue.profundus.lighting.LightSource
import in.dogue.profundus.Profundus

object Bat {
  private def mkAnim = {
    val tf = TileFactory(Color.Black, Color.Red.dim(2))
    val rightWing = Animation.create(Vector(
      (15, tf(CP437.¬)),
      (15, tf(CP437.^))

    ))
    val leftWing = Animation.create(Vector(
      (15, tf(CP437.⌐)),
      (15, tf(CP437.^))

    ))
    val body = Animation.singleton(tf(CP437.◦))
    Vector(
      ((-1,0), leftWing),
      ((0,0), body),
      ((1,0), rightWing)
    )
  }
  def create(ij:Cell, r:Random) = {
    val anim = mkAnim
    val light = LightSource.createCircle(ij, 0, 3, 0.2)
    StandardEntity.create[Bat](_.update, _.draw, StandardEntity.NoMove, Bat(anim), light, true, DamageType.HellBat.some, 12, r).toEntity(ij)
  }

}


case class Bat(a:AnimationGroup) {


  def update(health:Int, t:Int, args:EntityArgs):(Bat, Cell, Seq[GlobalMessage]) = {
    val pos = args.pos
    import Profundus._
    val isAdjacent = args.isAdjacent
    val newPos = if (args.isOnTop) {
      args.moveRandom
    } else if (!isAdjacent && t % 14 == 0 && args.hasLos) {
      val move =  args.toward
      move.map { m =>
        val res = pos |+| m
        if (!args.tc.isSolid(res)) {
          res
        } else {
          pos
        }
      }.getOrElse(pos)


    } else if (t % 15 == 0) {
      args.moveRandom
    } else {
      pos
    }
    val attack = if (isAdjacent && t % 15 == 0) {
      args.ppos.map{p => Seq(SingleTileZone(p, 75, DamageType.HellBat).toZone)}.getOrElse(Seq())
    } else {
      Seq()
    }

    val updated = copy(a=a.smap{_.update})


    (updated, newPos, attack.gss)
  }

  def draw(ij:Cell)(tr:TileRenderer):TileRenderer = {
    tr <++< a.map{ case (c, anim) => anim.drawFg(c |+| ij) _}
  }

}
