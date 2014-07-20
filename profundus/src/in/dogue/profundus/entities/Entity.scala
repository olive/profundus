package in.dogue.profundus.entities

import in.dogue.antiqua.graphics.TileRenderer
import in.dogue.antiqua.data.Direction
import in.dogue.profundus.world.{GlobalSpawn, WorldSpawn, TerrainCache, WorldTile}
import in.dogue.antiqua.Antiqua.Cell
import scala.util.Random
import in.dogue.profundus.particles.{Particle, DeathParticle}
import in.dogue.profundus.lighting.LightSource

case class Entity[T](ij:Cell,
                     fall:FallState,
                     up: T => (Cell, TerrainCache, Cell, LivingState, Random) => (T, Cell, Seq[GlobalSpawn], Seq[WorldSpawn]),
                     mv: T => (Cell, Direction, (Direction => Option[WorldTile])) => T,
                     dmg: T => Int => T,
                     doKill: T => T,
                     deathPart:T => Cell => Particle[_],
                     light: T => Cell => Seq[LightSource],
                     live:T => LivingState,
                     dr:T => Cell => TileRenderer => TileRenderer,
                     self:T) {
  def pos = ij
  def update(tc:TerrainCache, ppos:Cell, pState:LivingState, r:Random): (Entity[T], Seq[GlobalSpawn], Seq[WorldSpawn]) = {
    val (t, newPos, glob, worl) = up(self)(ij, tc, ppos, pState, r)
    (copy(self=t, ij=newPos), glob, worl)
  }

  def setFall(f:FallState): Entity[T] = copy(fall=f)
  def getFall = fall
  def move(to:Cell, dir:Direction, newSolid:(Direction => Option[WorldTile])) = {
    copy(ij=to, self=mv(self)(to, dir, newSolid))
  }

  def damage(amt:Int) = copy(self=dmg(self)(amt))

  def kill = copy(self=doKill(self))

  def getLiving = live(self)

  def draw(tr:TileRenderer):TileRenderer = tr <+< dr(self)(pos)
  def getLight = light(self)(pos)
  def getDeathParticle = deathPart(self)(pos)
  def toMassive:Massive[Entity[T]] = Massive(_.pos, _.move, (f:Entity[T]) => f.setFall, getFall, this)
}