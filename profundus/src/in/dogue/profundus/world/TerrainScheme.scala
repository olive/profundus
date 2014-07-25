package in.dogue.profundus.world

import com.deweyvm.gleany.graphics.Color
import scala.util.Random
import in.dogue.antiqua.data.{Direction, CP437}
import in.dogue.antiqua.graphics.{ColorHarmony, Tile}
import in.dogue.antiqua.Antiqua
import Antiqua._

object TerrainScheme {


  val emptyScheme = Scheme(
    (r:Random) => Color.Tan.dim(2 + r.nextDouble),
    (r:Random) => Color.Tan.dim(1 + r.nextDouble)
  )

  val dirtScheme = Scheme(
    (r:Random) => Color.Brown.dim(6 + r.nextDouble),
    (r:Random) => Color.Brown.dim(3 + r.nextDouble)
  )

  val clayScheme = {
    def r2(r:Random) = (1 - r.nextDouble).sq
    Scheme(
      (r:Random) => Color.Red.mix(Color.Brown, r2(r)).dim(6 + r.nextDouble),
      (r:Random) => Color.Red.mix(Color.Brown, r2(r)).dim(3 + r.nextDouble)
    )
  }

  val rockScheme = Scheme(
    (r:Random) => Color.Grey.dim(6 + r.nextDouble),
    (r:Random) => Color.Grey.dim(3 + r.nextDouble)
  )

  val rock2Scheme = Scheme(
    (r:Random) => Color.Grey.dim(10 + r.nextDouble),
    (r:Random) => Color.Grey.dim(6 + r.nextDouble)
  )

  val rock3Scheme = Scheme(
    (r:Random) => Color.Green.dim(6 + r.nextDouble),
    (r:Random) => Color.Green.dim(3 + r.nextDouble)
  )

  val shaftScheme = Scheme(
    (r:Random) => Color.Grey.dim(12 + r.nextDouble),
    (r:Random) => Color.Grey.dim(9 + r.nextDouble)
  )

  val gemScheme = Scheme(
    (r:Random) => Vector(Color.Purple, Color.Red, Color.Green, Color.Blue).randomR(r),
    (r:Random) => Color.Grey.dim(3 + r.nextDouble)
  )


  val dummy = TerrainScheme(new StratumColor(0), dirtScheme, clayScheme, rockScheme, rock2Scheme, rock3Scheme, gemScheme, shaftScheme, emptyScheme)


  def generate(r:Random) = {
    val hue = r.nextDouble
    val harmony = ColorHarmony.create(4, hue, 0.5f, 0.5f, 0, 0, 0.5f, 0.5f, 0.5f, r.nextInt())
    val dirt = harmony(0)
    val clay = harmony(1)
    val rock = harmony(2)
    val special = harmony(3)
    val emptyScheme =  Scheme(
      (r:Random) => dirt.mix(Color.Tan, r.nextDouble/2),
      (r:Random) => dirt.mix(Color.Tan, r.nextDouble/2)
    )
    val dirtScheme = Scheme(
      (r:Random) => dirt.dim(6 + r.nextDouble),
      (r:Random) => dirt.dim(3 + r.nextDouble)
    )
    val clayScheme = {
      Scheme(
        (r:Random) => clay.dim(6 + r.nextDouble),
        (r:Random) => clay.dim(3 + r.nextDouble)
      )
    }
    val rockScheme = Scheme(
      (r:Random) => rock.dim(6 + r.nextDouble),
      (r:Random) => rock.dim(3 + r.nextDouble)
    )
    val rock2Scheme = Scheme(
      (r:Random) => rock.dim(10 + r.nextDouble),
      (r:Random) => rock.dim(6 + r.nextDouble)
    )
    val rock3Scheme = Scheme(
      (r:Random) => special.dim(6 + r.nextDouble),
      (r:Random) => special.dim(3 + r.nextDouble)
    )
    val shaftScheme = Scheme(
      (r:Random) => rock.dim(12 + r.nextDouble),
      (r:Random) => rock.dim(9 + r.nextDouble)
    )
    val gemScheme = Scheme(
      (r:Random) => Vector(Color.Purple, Color.Red, Color.Green, Color.Blue).randomR(r),
      (r:Random) => rock.dim(3 + r.nextDouble)
    )
    val res = TerrainScheme(new StratumColor(hue), dirtScheme, clayScheme, rockScheme, rock2Scheme, rock3Scheme, gemScheme, shaftScheme, emptyScheme)
    res
  }
}

case class TerrainScheme(color:StratumColor,
                         dirtCs:Scheme,
                         clayCs:Scheme,
                         rock1Cs:Scheme,
                         rock2Cs:Scheme,
                         rock3Cs:Scheme,
                         mineralCs:Scheme,
                         shaftCs:Scheme,
                         emptyCs:Scheme) {
  self =>
  def toFactory(rand:Random):WorldTileFactory = new WorldTileFactory {
    override val ts: TerrainScheme = self
    override val r: Random = new Random(rand.nextInt())
  }

  def empty(r:Random) = {
    val bgCode = Vector(CP437.`.`, CP437.`,`, CP437.`'`, CP437.`"`).randomR(r)
    emptyCs.mkTile(r, bgCode)
  }

  def clay(r:Random) = {
    clayCs.mkTile(r, CP437.-)
  }

  def rock3(r:Random) = {
    rock3Cs.mkTile(r, CP437.`#`)
  }

  def rock2(r:Random) = {
    rock2Cs.mkTile(r, CP437.≡)
  }

  def rock1(r:Random) = {
    rock1Cs.mkTile(r, CP437.`=`)
  }

  def dirt(r:Random) = {
    dirtCs.mkTile(r, CP437.█)
  }

  def mineral(r:Random) = {
    val mineralTile = mineralCs.mkTile(r, CP437.◘)
    mineralTile @@ mineralTile.bgColor
  }

  def shaft(r:Random) = {
    val shaftCode = CP437.⌂
    shaftCs.mkTile(r, shaftCode)
  }

  def spike(d:Direction)(r:Random) = {
    import Direction._
    val code = d match {
      case Left => CP437.◄
      case Right => CP437.►
      case Down => CP437.▼
      case Up => CP437.▲
    }
    val empty = emptyCs.getBg(r)
    dirt(r).setCode(code).setBg(empty)
  }

}
