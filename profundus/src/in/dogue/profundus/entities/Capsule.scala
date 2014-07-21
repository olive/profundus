package in.dogue.profundus.entities

import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.antiqua.graphics.{Tile, TileRenderer, Animation}
import in.dogue.antiqua.data.{Direction, CP437}
import com.deweyvm.gleany.graphics.Color
import in.dogue.profundus.particles.{ExplosionParticle, Particle}
import in.dogue.profundus.deformations.{Deformation, ExplosionDeformation}
import in.dogue.profundus.world.{GlobalSpawn, WorldTile}
import in.dogue.profundus.entities.damagezones.{DamageZone, ExplosionZone}
import in.dogue.profundus.Profundus

object Capsule {
  val stick = CP437.║.mkTile(Color.Black, Color.Red.dim(2))
  def create(ij:Cell) = {
    val fuse = Animation.create(Vector(
      (1, CP437.*.mkTile(Color.Black, Color.Yellow)),
      (1, CP437.Θ.mkTile(Color.Black, Color.Red)),
      (1, CP437.☼.mkTile(Color.Black, Color.Orange))

    ))
    val stick = Animation.singleton(
      Capsule.stick
    )

    val anims = Seq(
      ((0, 0), stick),
      ((0, -1), fuse)
    )

    Capsule(ij, anims, Grounded, 0)
  }
}

case class Capsule private (ij:Cell, a:AnimationGroup, fall:FallState, t:Int){
  final val i = ij.x
  final val j = ij.y
  def pos = ij
  def move(p:Cell, from:Direction, newTouching:Direction => Option[WorldTile]) = copy(ij = p)
  def setState(f:FallState) = copy(fall=f)
  def update = {
    copy(a=a.smap {_.update}, t=t+1)
  }

  def isDone = t > 120

  def getDim = 1 + scala.math.sin(t/2.0)/4.0

  private def drawAura(tr:TileRenderer):TileRenderer = {
    val bound = (getDim*4).toInt
    val dist = (bound*bound).sqrt
    val draws = for (p <- (i - bound) to (i + bound);
                     q <- (j - bound) to (j + bound)) yield {
      def f(t:Tile) = t.setFg(t.fgColor.dim(1/getDim))
      val func = if (scala.math.hypot(i - p, j - q) < dist) {
        f _
      } else {
        id[Tile] _
      }
      ((p, q), func)
    }
    tr `$$>` draws
  }

  private def makeDeformation = {
    ExplosionDeformation.create(ij, 1, 8, 3).toDeformation
  }

  private def makeParticle = {
    ExplosionParticle.create(ij, 8, 3).toParticle
  }

  private def makeZone = {
    ExplosionZone.create(ij, 8, 3, DamageType.Explosion).toZone
  }


  def getExplode:Seq[GlobalSpawn] = {
    import Profundus._
    Seq(Seq(makeDeformation).gs,
        Seq(makeParticle).gs,
        Seq(makeZone).gs)
  }

  def draw(tr:TileRenderer):TileRenderer = {
    tr <## (a |+| ij) <+< drawAura
  }

  def toMassive:Massive[Capsule] = Massive(_.pos, _.move, _.setState, fall, this)

}
