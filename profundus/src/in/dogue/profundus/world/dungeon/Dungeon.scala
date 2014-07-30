package in.dogue.profundus.world.dungeon

import in.dogue.antiqua.data.{Graph, Array2d, Direction}
import scala.util.Random
import java.awt.image.BufferedImage
import com.deweyvm.gleany.graphics.Color
import javax.imageio.ImageIO
import java.io.File
import in.dogue.antiqua.Antiqua._
import scala.Some
import scala.annotation.tailrec
import in.dogue.antiqua.data

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
  def isBlank = !Direction.All.exists { d => open(d)}

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


object Dungeon {
  import DungeonCell._
  private def createRaw(cols:Int, rows:Int, r:Random) = {
    val p = cols/4 + r.nextInt(cols/2)
    val q = rows/4 + r.nextInt(rows/2)
    val indices = r.shuffle(for (i <- 0 until cols; j <- 0 until rows) yield (i, j))
    val seed = Array2d.tabulate(cols, rows) { case (x, y) =>
      DungeonCell.allOpen(r).onlyIf(x == p && y == q)
    }

    val cells = indices.foldLeft(seed) { case (arr, p) =>
      val t = arr.get(p) match {
        case None => choose(DungeonCell.allOpen(r), arr, p, r)
        case Some(dc) => choose(dc, arr, p, r)
      }
      arr.updated(p, t.some)
    }

    val filledCells = cells.map { case (p, opt) =>
      opt.fold(DungeonCell.blank)(id[DungeonCell])
    }
    Dungeon(cols, rows, filledCells)
  }

  private def count(arr:Array2d[DungeonCell]) = {
    arr.foldLeft(0) { case (acc, (pt, dc)) =>
      acc + (if (!dc.isBlank) {
        1
      } else {
        0
      })
    }
  }

  def create(cols:Int, rows:Int, minFill:Double, r:Random) = {
    var ct = 0
    var dun = createRaw(cols, rows, r)
    while (ct.toFloat/(cols*rows) < minFill) {
      val seed = (r.nextInt(cols), r.nextInt(rows))

      dun = createRaw(cols, rows, r)
      dun = dun.floodDungeon(seed)
      ct = count(dun.cells)
    }
    openBottom(r)(openTop(pare(dun)))

  }

  def choose(dc:DungeonCell, arr:Array2d[Option[DungeonCell]], p:Cell, r:Random) = {
    def get(d:Direction) = {
      arr.getOption(p --> d).flatten.map{_.open(d.opposite)}.getOrElse(r.nextBoolean()) && dc.open(d) && arr.getOption(p --> d).isDefined
    }

    def getJunc(d:Direction) = {
      arr.getOption(p --> d).flatten.map{_.junc(d.opposite)}.getOrElse(Juncture.All.randomR(r))
    }

    dc.mod(dmap(get), dmap(getJunc))
  }

  def pare(d:Dungeon) = {
    val span = d.cells.flatten.getFilterSpan(c => !c.isBlank)
    val root = (span.x, span.y)
    Dungeon(span.width, span.height, Array2d.tabulate(span.width, span.height) { case p =>
      d.cells.get(p |+| root)
    })
  }

  def openTop(d:Dungeon):Dungeon = {
    val fs = (for (i <- 0 until d.cols) yield {
      val found = (0 until d.rows).find{ j => !d.cells.get((i, j)).isBlank }
      found.map { f => (i,f)}
    }).flatten

    fs.foldLeft(d) { case (acc, p) =>
      acc.modAt(p, Direction.Up, true)
    }
  }

  def openBottom(r:Random)(d:Dungeon):Dungeon = {
    val bot = d.rows - 1
    val fs = r.shuffle(for (i <- 0 until d.cols) yield {
      (i, bot).onlyIf(!d.cells.get((i, bot)).isBlank)
    })
    fs.flatten.headOption.map { f => d.modAt(f, Direction.Down, true)}.getOrElse{
      throw new RuntimeException("Dungeon has no exit!")
    }
  }
}


case class Dungeon(cols:Int, rows:Int, cells:Array2d[DungeonCell]) {

  def toGraph:Graph[Cell, DungeonCell] = new Graph[Cell, DungeonCell] {
    def get(c:Cell) = cells.get(c)
    def getNeighbors(p:Cell) = {
      if (cells.get(p).isBlank) {
        Direction.All.filter{
          d => cells.getOption(p --> d).exists(_.isBlank)
        }.map {
          d => p --> d
        }
      } else {
        Direction.All.filter{
          d => cells.getOption(p --> d).exists(c => c.open(d.opposite))
        }.map {
          d => p --> d
        }
      }
    }
  }

  def modAt(p:Cell, d:Direction, v:Boolean) = {
    copy(cells=cells.update(p, _.modDir(d, v)))
  }

  def floodDungeon(seed:Cell) = {
    val m = Graph.flood(toGraph, seed)
    Dungeon(cols, rows, Array2d.tabulate(cols, rows) { case p =>
      m.contains(p).select(DungeonCell.blank, cells.get(p))
    })
  }


  private def getInterior(s:Seq[Set[Cell]]):Seq[Set[Cell]] = {
    val spans = s.map { set => set -> set.toSeq.map { a => (a, a)}.getSpan}
    spans.filter { case (set, span) =>
      val edge = span.x == 0 || span.y == 0 || span.x + span.width == cols  || span.y + span.height == rows
      val blank = cells.get(set.head).isBlank
      blank && !edge
    }.map{_._1}
  }

  def getMask = {
    val allIndices = Set((for (i <- 0 until cols ; j <- 0 until rows) yield (i, j)):_*)
    val all: Seq[Set[Cell]] = getInterior(Graph.floodAll(toGraph, allIndices))
    val solid = Array2d.tabulate(cols, rows) { case p =>
      cells.get(p).solidify(p, all)
    }
    val size = DungeonCell.cellSize
    Array2d.tabulate(cols*size, rows*size) { case (x, y) =>
      val index = (x/size, y/size)
      val off = (x % size, y % size)
      solid.get(index).get(off)

    }
  }

  def saveImage(r:Random) {

    val zoom:Int = cells.first.map { c => c.size}.getOrElse(11)
    val img = new BufferedImage(cells.cols*zoom, cells.rows*zoom, BufferedImage.TYPE_INT_RGB)
    val mask = getMask
    mask.foreach { case (cell, b) =>
      val solid = Color.Red.toLibgdxColor.toIntBits >> 8
      val clear = Color.White.toLibgdxColor.toIntBits
      img.setRGB(cell.x, cell.y, b.select(clear, solid))
    }
    val out = new File("saved.png")
    if (!ImageIO.write(img, "png", out)) {
      println("hello")
    }
  }


}
