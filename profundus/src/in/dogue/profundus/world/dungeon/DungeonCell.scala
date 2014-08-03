package in.dogue.profundus.world.dungeon

import in.dogue.antiqua.data.{Array2d, Direction}
import scala.util.Random
import in.dogue.antiqua.Antiqua._
import in.dogue.profundus.world.GlobalMessage
import in.dogue.profundus.entities.Ladder
import in.dogue.profundus.Profundus

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
  val doorSize = 3

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
sealed trait CellType
case object Exterior extends CellType
case object Interior extends CellType
case object Wall extends CellType
case object Blocked extends CellType
case object ConeSpace extends CellType
case class LadderSpec(height:Int, pos:Cell)
case class ReifiedCell(tiles:Array2d[CellType], open:Direction=>Boolean, junc:Direction=>Juncture, ladders:Seq[LadderSpec]) {
  import Profundus._
  def getMessages:Seq[GlobalMessage] = {
    ladders.map{spec => Ladder.create(spec.pos, spec.height).toClimbable}.gms
  }
  def get(p:Cell) = tiles.get(p)
}

case class DungeonCell(size:Int, open:Direction=>Boolean, junc:Direction=>Juncture) {
  import DungeonCell._
  def isBlank = !Direction.All.exists(open)

  def getOffset(j:Juncture) = j match {
    case Mid => size/2
    case Low => size - 3
    case High => 2
  }

  private def stamp(d:Direction, f:((Int,Int) => Cell), arr:Array2d[Boolean]) = {
    val offset = getOffset(junc(d))
    ((-doorSize/2) to (doorSize/2)).foldLeft(arr) { case (acc, p) =>
      val pt = f(p, offset)
      acc.updated(pt, false)
    }
  }

  def reify(absPos:Cell/*for messages*/, ij:Cell, interior:Seq[Set[Cell]]):(ReifiedCell) = {
    if (interior.exists { set => set.contains(ij)}) {
      val tiles = Array2d.tabulate(size, size) { case p =>
        Blocked : CellType
      }
      return ReifiedCell(tiles, _ => false, _ => Mid, Seq())
    }
    if (isBlank) {
      val tiles = Array2d.tabulate(size, size) { case p =>
        Exterior : CellType
      }
      return ReifiedCell(tiles, _ => true, _ => Mid, Seq())
    }
    val first = Array2d.tabulate(size, size) { case (i, j) =>
      i == 0 || i == size -1 || j == 0 || j == size - 1
    }

    val pts = Map(
      Direction.Up -> ((p:Int, offset:Int) => (p + offset, 0)),
      Direction.Down -> ((p:Int, offset:Int) => (p + offset, size - 1)),
      Direction.Left -> ((p:Int, offset:Int) => (0, p + offset)),
      Direction.Right -> ((p:Int, offset:Int) => (size - 1, p + offset))
    )

    val tiles = pts.filter { case (d, _) => open(d) }.foldLeft(first) { case (arr, (d, f)) =>
      stamp(d, f, arr)
    }.map { case (_, t) =>
      t.select(Interior, Wall) : CellType
    }

    val off = (ij.x * size, ij.y*size) |+| absPos
    val leftPos = (1,1) |+| off
    val left = LadderSpec(size - 2, leftPos).onlyIfs(open(Direction.Left))

    val rightPos = (size - 2, 1) |+| off
    val right = LadderSpec(size - 2, rightPos).onlyIfs(open(Direction.Right))

    val upPos = (1, 1) |+| off
    val offset = getOffset(junc(Direction.Up)) - 1
    val up = LadderSpec(size + 3, (upPos |+ offset) -| 5).onlyIfs(open(Direction.Up))

    ReifiedCell(tiles, open, junc, left ++ right ++ up)

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
