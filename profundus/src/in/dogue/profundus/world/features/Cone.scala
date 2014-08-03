package in.dogue.profundus.world.features

import in.dogue.antiqua.data.Array2d
import in.dogue.profundus.Profundus
import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.Antiqua
import in.dogue.profundus.world.dungeon._
import in.dogue.antiqua.Antiqua.Cell

case class Cone(top00:Int, top11:Int, bottom00:Int, bottom11:Int, xy:Cell, d:Dungeon, cols:Int, rows:Int) {
  import Antiqua._
  val t0 = top00 + xy.x
  val t1 = top11 + xy.x
  val b0 = bottom00 + xy.x
  val b1 = bottom11 + xy.x
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

  private def btc(b:Boolean) = b.select(Color.White, Color.Black)



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
  val (mask, _) = d.getMask(48, (20,0))
  val testMask = Array2d.tabulate(cols, rows*2 + d.rows*DungeonCell.cellSize) { case (x, y) =>
    if (y < rows) {
      topMask.get((x, y))
    } else if (y >= rows + d.rows*11) {
      bottomMask.get((x, y - (rows + d.rows*11)))
    } else {
      val p = mask.getOption((x,  y - rows) |-| xy)
      p match {
        case Some(r) => r match {
          case Wall => true
          case Interior => false
          case Exterior => true
          case Blocked => true
        }
        case None => true
      }
    }
  }.render(btc, "topbottom.png")

}
