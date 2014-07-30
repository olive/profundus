package in.dogue.profundus.entities

import in.dogue.antiqua.Antiqua._
import in.dogue.profundus.world.{TerrainCache, Unloadable, GlobalMessage}
import in.dogue.antiqua.graphics.TileRenderer


object Climbable {
  def apply[A](acnts: A => Cell => Boolean,
               aisTop: A => Cell => Boolean,
               akillableAt: A => Cell => Boolean,
               adoKill: A => A,
               aup: A => TerrainCache => (Option[A], Seq[GlobalMessage]),
               adr: A => TileRenderer => TileRenderer,
               agetPos: A => Cell,
               aself: A) = new Climbable {
    override type T = A
    override val cnts = acnts
    override val isTop = aisTop
    override val killableAt = akillableAt
    override val doKill = adoKill
    override val up = aup
    override val dr = adr
    override val getPos = agetPos
    override val self = aself
  }
}
trait Climbable {
  type T
  val cnts: T => Cell => Boolean
  val isTop: T => Cell => Boolean
  val killableAt: T => Cell => Boolean
  val doKill: T => T
  val up: T => TerrainCache => (Option[T], Seq[GlobalMessage])
  val dr: T => TileRenderer => TileRenderer
  val getPos: T => Cell
  val self: T

  private def updateSelf(ns:T) = Climbable(cnts, isTop, killableAt, doKill, up, dr, getPos, ns)

  def contains(ij:Cell) = cnts(self)(ij)
  def isKillableAt(ij:Cell) = killableAt(self)(ij)

  def kill = updateSelf(doKill(self))
  def update(tc:TerrainCache) = {
    val (ns, msgs) = up(self)(tc)
    ns.map { n => updateSelf(n)} @@ msgs
  }

  def draw(tr:TileRenderer):TileRenderer = {
    tr <+< dr(self)
  }

  def getPosition = getPos(self)
  def toUnloadable:Unloadable[Climbable] = Unloadable.fromPos(this, _.getPosition)
}
