package in.dogue.profundus.world.features

import in.dogue.antiqua.data.Array2d
import in.dogue.antiqua.Antiqua
import in.dogue.profundus.world.dungeon._
import in.dogue.antiqua.Antiqua.Cell

case class Cone(top0:Int, top1:Int, bottom0:Int, bottom1:Int, xy:Cell, top:Cell, bottom:Cell, cols:Int, rows:Int) {
  import Antiqua._
  val t0 = top0 + xy.x
  val t1 = top1 + xy.x
  val b0 = bottom0 + xy.x
  val b1 = bottom1 + xy.x
  private val topLeft = Array2d.tabulate(t0, rows) { case (x, y) =>
    y > (rows/t0.sq)*x.sq
  }

  private val topRight = Array2d.tabulate(cols - t1, rows) { case (x, y) =>
    val run = cols - t1
    y > (rows/run.sq)*(run - x).sq
  }

  private val bottomLeft = Array2d.tabulate(b0, rows) { case (x, y) =>
    val yy = rows - y
    yy > (rows/b0.sq)*x.sq
  }

  private val bottomRight =  Array2d.tabulate(cols - b1, rows) { case (x, y) =>
    val yy = rows - y
    val run = cols - b1
    yy > (rows/run.sq)*(run - x).sq
  }

  val topMask = Array2d.tabulate(cols, rows) { case (x, y) =>
    if (x < t0) {
      topLeft.get((x, y))
    } else if (x < t1) {
      false
    } else {
      topRight.get((x - t1, y))
    }
  }

  val bottomMask = Array2d.tabulate(cols, rows) { case (x, y) =>
    if (x < b0) {
      bottomLeft.get((x, y))
    } else if (x < b1) {
      false
    } else {
      bottomRight.get((x - b1, y))
    }
  }

  def getMask(dcols:Int, drows:Int, dTiles:Array2d[CellType]) = {
    Array2d.tabulate(cols, rows*2 + drows*DungeonCell.cellSize) { case (x, y) =>
      if (y < rows) {
        topMask.get((x, y)).select(ConeSpace,Blocked)
      } else if (y >= rows + drows*DungeonCell.cellSize) {
        bottomMask.get((x, y - (rows + drows*11))).select(ConeSpace,Blocked)
      } else {
        dTiles.getOption((x,  y - rows) |- xy.x).getOrElse(Blocked)
      }
    }
  }

}
