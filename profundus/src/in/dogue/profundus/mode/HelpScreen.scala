package in.dogue.profundus.mode

import in.dogue.antiqua.Antiqua.TileGroup
import in.dogue.antiqua.graphics.TileRenderer
import in.dogue.profundus.Profundus
import Profundus._
import in.dogue.antiqua.Antiqua
import Antiqua._


object HelpScreen {
  def create = {
    val tf = Profundus.tf
    val x0 = 5
    val y0 = 18
    val p0 = (x0, y0)
    val all = Seq(
      "Movement     ↑↓→←",
      "Use tool     SPACE",
      "Climb        C",
      "Capsule      X",
      "Cable        Z",
      "Drop tool    V",
      "Light flare  ↑+C",
      "Wall kick    LEFT_SHIFT",
      "Show help    H",
      "Show perf    TAB"

    ).map {
      case s => tf.create(s).toTileGroup
    }.zipWithIndex.map {
      case (tp, k) => tp |++| (p0 +| k)
    }
    HelpScreen(all.flatten)
  }

}

case class HelpScreen(grs:TileGroup) {
  def draw(tr:TileRenderer):TileRenderer = {
    tr <++ grs
  }
}
