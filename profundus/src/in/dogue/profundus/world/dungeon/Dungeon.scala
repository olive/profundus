package in.dogue.profundus.world.dungeon

import in.dogue.antiqua.data.{Graph, Array2d, Direction}
import scala.util.Random
import java.awt.image.BufferedImage
import com.deweyvm.gleany.graphics.Color
import javax.imageio.ImageIO
import java.io.File
import in.dogue.antiqua.Antiqua._
import scala.Some
import in.dogue.profundus.world.GlobalMessage
import in.dogue.profundus.Profundus
import in.dogue.profundus.world.features.Cone
import com.badlogic.gdx.Gdx


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

    val filledCells = cells.map { case (_, opt) =>
      opt.fold(DungeonCell.blank)(id[DungeonCell])
    }
    Dungeon(cols, rows, filledCells)
  }

  private def count(arr:Array2d[DungeonCell]) = {
    arr.countT( dc => !dc.isBlank)
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
    val (dTop, top) = openTop(r)(pare(dun))
    val (d, bottom) = openBottom(r)(dTop)
    (d, top, bottom)
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

  def openTop(r:Random)(d:Dungeon):(Dungeon, (Int,Int)) = {
    val fs = (for (i <- 0 until d.cols) yield {
      val found = (0 until d.rows).find{ j => !d.cells.get((i, j)).isBlank }
      found.map { f => (i,f)}
    }).flatten
    val tops = r.shuffle(fs.filter(p => p.y == 0))
    val top = tops.headOption.getOrElse(throw new RuntimeException("Dungeon has no top"))
    val dd = d.modAt(top, Direction.Up, true)
    (dd, top)
  }

  def openBottom(r:Random)(d:Dungeon):(Dungeon, (Int,Int)) = {
    val bot = d.rows - 1
    val fs = r.shuffle(for (i <- 0 until d.cols) yield {
      (i, bot).onlyIf(!d.cells.get((i, bot)).isBlank)
    })
    val bottom = fs.flatten.headOption.getOrElse{
      throw new RuntimeException("Dungeon has no exit!")
    }
    d.modAt(bottom, Direction.Down, true) @@ bottom
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

  def getMask(worldRows:Int, xy:Cell, top:Cell, bottom:Cell): (Array2d[CellType], Cone, Map[Int, Seq[GlobalMessage]]) = {
    val allIndices = Set((for (i <- 0 until cols ; j <- 0 until rows) yield (i, j)):_*)
    val all: Seq[Set[Cell]] = getInterior(Graph.floodAll(toGraph, allIndices))
    val size = DungeonCell.cellSize
    val reified = Array2d.tabulate(cols, rows) { case p =>
      //fixme -- offset by xy below with Cone, not here
      cells.get(p).reify(xy +| worldRows/* accounts for cone*/, p, all)
    }
    val msgArray = reified.map { case (p, r) =>
      r.getMessages
    }
    val seed = Map[Int, Seq[GlobalMessage]]().withDefaultValue(Seq())
    val msgs = msgArray.foldLeft(seed) { case (map, (p, ms)) =>
      val y = (p.y*size + xy.y)/worldRows + 1
      println(y)
      map.updated(y, map(y) ++ ms)
    }
    val tiles = Array2d.tabulate(cols*size, rows*size) { case (x, y) =>
      val index = (x/size, y/size)
      val off = (x % size, y % size)
      reified.get(index).get(off)
    }

    tiles.render("out.png") {
      case Exterior => Color.Black
      case Interior => Color.White
      case _ => Color.Black
    }

    val cs = DungeonCell.cellSize
    val doorSize = DungeonCell.doorSize
    val topCell = cells.get(top)
    val topOff = topCell.getOffset(topCell.junc(Direction.Up))
    val top0 = top.x*cs + topOff - 1
    val top1 = top.x*cs + topOff + doorSize - 1
    val bottomCell = cells.get(bottom)
    val bottomOff = bottomCell.getOffset(bottomCell.junc(Direction.Down))
    val bottom0 = bottom.x*cs + bottomOff - 1
    val bottom1 = bottom.x*cs + bottomOff + doorSize - 1
    val cone = new Cone(top0, top1, bottom0, bottom1, xy, top, bottom, 128, 48)

    tiles @@ cone @@ msgs
  }


}
