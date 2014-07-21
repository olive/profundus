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
                         dirt:Scheme,
                         clay:Scheme,
                         rock:Scheme,
                         rock2:Scheme,
                         rock3:Scheme,
                         gem:Scheme,
                         shaft:Scheme,
                         empty:Scheme) {
  /*def map(f:Color => Color) = copy(
    dirt=dirt.map(f),
    clay=clay.map(f),
    rock=rock.map(f),
    rock2=rock2.map(f),
    rock3=rock3.map(f),
    gem=gem.map(f),
    shaft=shaft.map(f),
    empty=empty.map(f)
  )*/
  def emptyTile(r:Random) = {
    val bgCode = Vector(CP437.`.`, CP437.`,`, CP437.`'`, CP437.`"`).randomR(r)
    empty.mkTile(r, bgCode)
  }

  def makeEmpty(r:Random) = {
    val empty = emptyTile(r)
    Empty(empty, true)
  }

  def makeClay(r:Random) = {
    val clayTile = clay.mkTile(r, CP437.-)
    val empty = emptyTile(r)
    Clay.create(clayTile, empty)
  }

  def makeRock3(r:Random) = {
    val rockTile = rock3.mkTile(r, CP437.`#`)
    val empty = emptyTile(r)
    Rock3.create(rockTile, empty)
  }

  def makeRock2(r:Random) = {
    val rockTile = rock2.mkTile(r, CP437.≡)
    val empty = emptyTile(r)
    Rock2.create(rockTile, empty)
  }

  def makeRock(r:Random) = {
    val rockTile = rock.mkTile(r, CP437.`=`)
    val empty = emptyTile(r)
    Rock.create(rockTile, empty)
  }

  def makeDirt(r:Random) = {
    val dirtTile = dirt.mkTile(r, CP437.█)
    val empty = emptyTile(r)
    Dirt.create(dirtTile, empty)

  }

  def makeMineral(r:Random) = {
    val mineralTile = gem.mkTile(r, CP437.◘)
    val empty = emptyTile(r)
    Mineral.create(mineralTile, empty, mineralTile.bgColor)
  }

  def makeShaft(r:Random) = {
    val shaftCode = CP437.⌂
    val shaftTile = shaft.mkTile(r, shaftCode)
    Shaft.create(shaftTile)
  }

  def makeSpike(d:Direction)(r:Random) = {
    import Direction._
    val code = d match {
      case Left => CP437.◄
      case Right => CP437.►
      case Down => CP437.▼
      case Up => CP437.▲
    }
    val empty = this.empty.getBg(r)
    val tile: Tile = makeDirt(r).tile.setCode(code).setBg(empty)
    val emptyTile = CP437.` `.mkTile(empty, empty)
    Spike.create(tile, emptyTile, d)
  }

}
