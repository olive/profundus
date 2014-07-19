package in.dogue.profundus.world

import com.deweyvm.gleany.graphics.Color
import scala.util.Random
import in.dogue.antiqua.data.{Direction, CP437}
import in.dogue.antiqua.graphics.Tile
import in.dogue.antiqua.Antiqua
import Antiqua._

case class TerrainScheme(dirt:Scheme, rock:Scheme, rock2:Scheme, rock3:Scheme, gem:Scheme, clay:Scheme, shaft:Scheme, empty:Scheme) {
  def map(f:Color => Color) = copy(
    dirt.map(f),
    rock.map(f),
    rock2.map(f),
    rock3.map(f),
    gem.map(f),
    clay.map(f),
    shaft.map(f),
    empty.map(f)
  )
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
