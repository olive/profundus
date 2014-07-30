package in.dogue.profundus.world.dungeon

import in.dogue.antiqua.data.{Array2d, Direction}
import scala.util.Random
import in.dogue.antiqua.Antiqua._

object Juncture {
  val All = Vector(High, Mid, Low)
}
sealed trait Juncture
case object High extends Juncture
case object Mid extends Juncture
case object Low extends Juncture

object DungeonCell {
  def dmap[T](f:Direction => T):Map[Direction, T] = {
    Direction.All.map { d => d -> f(d)}.toMap
  }
  val cellSize = 11
  private def randJunc(r:Random):Map[Direction, Juncture] = {
    dmap(d => Juncture.All.randomR(r))
  }
  def mkCell(r:Random) = {
    DungeonCell(
      cellSize,
      dmap(d => r.nextBoolean),
      randJunc(r)
    )
  }

  def allOpen(r:Random) = DungeonCell(cellSize,_ => true, randJunc(r))

  def blank = DungeonCell(cellSize,_ => false, _ => Mid)


}

case class DungeonCell(size:Int, open:Direction=>Boolean, junc:Direction=>Juncture) {
  import DungeonCell._
  def isBlank = !Direction.All.exists(open)

  private def getOffset(j:Juncture) = j match {
    case Mid => size/2
    case Low => size - 3
    case High => 2
  }

  private def stamp(d:Direction, f:((Int,Int) => Cell), arr:Array2d[Boolean]) = {
    val offset = getOffset(junc(d))
    (-1 to 1).foldLeft(arr) { case (acc, p) =>
      val pt = f(p, offset)
      acc.updated(pt, false)
    }
  }

  def solidify(ij:Cell, interior:Seq[Set[Cell]]):Array2d[Boolean] = {
    if (interior.exists { set => set.contains(ij)}) {
      return Array2d.tabulate(size, size) { case p =>
        true
      }
    }
    if (isBlank) {
      return Array2d.tabulate(size, size) { case p =>
        false
      }
    }
    val first = Array2d.tabulate(size, size) { case (i, j) =>
      i == 0 || i == size -1 || j == 0 || j == size - 1
    }

    val pts = Map(
      Direction.Up -> ((p:Int, offset:Int) => (p + offset, 0)),
      Direction.Down -> ((p:Int, offset:Int) => (p + offset, size - 1)),
      Direction.Left -> ((p:Int, offset:Int) =>(0, p + offset)),
      Direction.Right -> ((p:Int, offset:Int) =>(size - 1, p + offset))
    )

    pts.filter { case (d, _) => open(d) }.foldLeft(first) { case (arr, (d, f)) =>
      stamp(d, f, arr)
    }

  }

  def mod(open:Direction=>Boolean, junc:Direction=>Juncture) = {
    copy(open=open, junc=junc)
  }

  def modDir(d:Direction, v:Boolean) = {

    val map:Map[Direction, Boolean] = dmap{ dd =>
      if (dd == d) {
        v
      } else {
        open(dd)
      }
    }
    DungeonCell(size, map, junc)
  }

}
