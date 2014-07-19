package in.dogue.profundus.world

import scala.util.Random
import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.data.Code

case class Scheme(bgMod:Random => Color,
                  fgMod:Random => Color) {
  def getBg(r:Random) = bgMod(r)
  def getFg(r:Random) = fgMod(r)
  def mkTile(r:Random, c:Code) = c.mkTile(getBg(r), getFg(r))
  def map(f:Color => Color) = {
    def bgModp(r:Random) = f(bgMod(r))
    def fgModp(r:Random) = f(fgMod(r))
    Scheme(bgModp, fgModp)
  }
}
