package in.dogue.profundus.world.features

import in.dogue.antiqua.data.Array2d
import in.dogue.profundus.Profundus
import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.Antiqua

case class Cone(exit0:Int, exit1:Int, cols:Int, rows:Int) {
  import Profundus._
  import Antiqua._
  private val left = Array2d.tabulate(exit0, rows) { case (x, y) =>
    y > (rows/exit0.sq)*x.sq
  }

  private val right = Array2d.tabulate(cols - exit1, rows) { case (x, y) =>
    val run = cols - exit1
    y > (rows/run.sq)*(run - x).sq
  }

  private def btc(b:Boolean) = b.select(Color.White, Color.Black)

  val mask = Array2d.tabulate(cols, rows) { case (x, y) =>
    if (x < exit0) {
      left.get((x, y))
    } else if (x < exit1) {
      false
    } else {
      right.get((x - exit1, y))
    }
  }

}
