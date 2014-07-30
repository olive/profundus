package in.dogue.profundus.world.dungeon

import in.dogue.antiqua.data.{Array2d, Direction}
import scala.util.Random
import java.awt.image.BufferedImage
import com.deweyvm.gleany.graphics.Color
import javax.imageio.ImageIO
import java.io.File
import in.dogue.antiqua.Antiqua._
import scala.Some

object DungeonCell {
  def mkCell(r:Random) = {
    DungeonCell(Direction.All.map { d => d -> r.nextBoolean}.toMap)
  }

  def allOpen = DungeonCell(_ => true)

  def blank = DungeonCell(_ => false)
}

case class DungeonCell(open:Direction=>Boolean) {
  def isBlank = !Direction.All.exists { d => open(d)}

  def mod(d:Direction, v:Boolean) = {

    val map:Map[Direction, Boolean] = Direction.All.map { dd =>
      dd -> (if (dd == d) {
        v
      } else {
        open(dd)
      })
    }.toMap
    DungeonCell(map)
  }
}


object Dungeon {

  private def createRaw(cols:Int, rows:Int, r:Random) = {
    val p = cols/4 + r.nextInt(cols/2)
    val q = rows/4 + r.nextInt(rows/2)
    val indices = r.shuffle(for (i <- 0 until cols; j <- 0 until rows) yield (i, j))
    val seed = Array2d.tabulate(cols, rows) { case (x, y) =>
      DungeonCell.allOpen.onlyIf(x == p && y == q)
    }

    val cells = indices.foldLeft(seed) { case (arr, p) =>
      val t = arr.get(p) match {
        case None => choose(DungeonCell.allOpen, arr, p, r)
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
      dun = dun.flood((seed, dun.cells.get(seed)))
      ct = count(dun.cells)
    }
    pare(dun)

  }

  def choose(dc:DungeonCell, arr:Array2d[Option[DungeonCell]], p:Cell, r:Random) = {
    def get(d:Direction) = {
      arr.getOption(p --> d).flatten.map{_.open(d.opposite)}.getOrElse(r.nextBoolean()) && dc.open(d) && arr.getOption(p --> d).isDefined
    }

    DungeonCell(Direction.All.map{d => d -> get(d)}.toMap)
  }

  def pare(d:Dungeon) = {
    val span = d.cells.flatten.getFilterSpan(c => !c.isBlank)
    val root = (span.x, span.y)
    Dungeon(span.width, span.height, Array2d.tabulate(span.width, span.height) { case p =>
      d.cells.get(p |+| root)
    })
  }
}
case class Dungeon(cols:Int, rows:Int, cells:Array2d[DungeonCell]) {

  def flood(seed:(Cell, DungeonCell)) = {
    var set = Set[(Cell, DungeonCell)]() + seed
    def takeSet[T](set:Set[T]) = {
      val first = set.head
      val rest = set - first
      (first, rest)
    }
    def getNeighbors(p:Cell) = {
      Direction.All.filter{
        d => cells.getOption(p --> d).exists(c => c.open(d.opposite))
      }.map {
        d => (p --> d) -> cells.get(p --> d)
      }
    }
    val filled = collection.mutable.Map[Cell,DungeonCell]()
    while (!set.isEmpty) {
      val ((pos, dc), newSet) = takeSet(set)
      filled(pos) = dc
      set = newSet
      val ns = getNeighbors(pos)
      ns foreach { case n@(ppos, _) =>
        if (!filled.contains(ppos)) {
          set = set + n
        }

      }
    }
    Dungeon(cols, rows, Array2d.tabulate(cols, rows) { case p =>
      filled.get(p).getOrElse(DungeonCell.blank)
    })
  }

  def saveImage(r:Random) {
    val zoom = 9
    val img = new BufferedImage(cells.cols*zoom, cells.rows*zoom, BufferedImage.TYPE_INT_RGB)

    cells.foreach { case (cell, dc) =>
      val x = cell.x*zoom
      val y = cell.y*zoom
      for (i <- 0 until zoom; j <- 0 until zoom) {
        def open(d:Direction) = dc.open(d)
        val solid = Color.Red.toLibgdxColor.toIntBits >> 8
        val clear = Color.White.toLibgdxColor.toIntBits
        if (i == 0 || j == 0 || i == zoom - 1 || j == zoom - 1) {
          img.setRGB(x + i, y + j, solid)
        } else {
          img.setRGB(x + i, y + j, clear)
        }
        val inds = -2 to 2
        if (dc.isBlank) {
          img.setRGB(x + i, y + j, solid)
        }
        if (open(Direction.Up)) {
          inds.foreach { i =>
            img.setRGB(x + zoom/2 +i, y, clear)
          }

        }
        if (open(Direction.Down)) {
          inds.foreach { i =>
            img.setRGB(x + zoom/2 + i, y + zoom-1, clear)
          }

        }
        if (open(Direction.Left)) {
          inds.foreach { i =>
            img.setRGB(x, y + zoom/2 + i, clear)
          }

        }
        if (open(Direction.Right)) {
          inds.foreach { i =>
            img.setRGB(x + zoom-1, y + zoom/2 + i, clear)
          }

        }
        ()
      }

    }
    val outputfile = new File("saved.png")
    if (!ImageIO.write(img, "png", outputfile)) {
      println("hello")
    }
  }


}
