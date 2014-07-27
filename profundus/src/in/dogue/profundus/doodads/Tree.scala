package in.dogue.profundus.doodads

import in.dogue.antiqua.data.{Code, CP437}
import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.Antiqua
import Antiqua._
import scala.util.Random
import in.dogue.antiqua.graphics.{Tile, TileRenderer}
import scala.collection.immutable.IndexedSeq


object Segment {
  val All = Vector(Left, Right, LeftRight, Neither)
  sealed trait SegmentType {
    val trunk:Code
    val hasLeft:Boolean = false
    val hasRight:Boolean = true
  }
  case object Left extends SegmentType {
    override val trunk = CP437.╢.toCode
    override val hasLeft = true
  }
  case object Right extends SegmentType {
    override val trunk = CP437.╟.toCode
    override val hasRight = true
  }
  case object LeftRight extends SegmentType {
    override val trunk = CP437.╫.toCode
    override val hasLeft = true
    override val hasRight = true
  }
  case object Neither extends SegmentType {
    override val trunk = CP437.║.toCode
  }

}

object Tree {
  def create(xy:Cell, barkColor:Color, leafColor:Color, r:Random) = {
    val height = 4 + r.nextInt(12)
    val leftLeaf = CP437./.mkTile(Color.Black, leafColor)
    val midLeaf = CP437.|.mkTile(Color.Black, leafColor)
    val rightLeaf = CP437.\.mkTile(Color.Black, leafColor)
    val top = Vector(
      ((0,0), leftLeaf),
      ((1,0), midLeaf),
      ((2,0), rightLeaf)
    )
    val trunk = Vector(
      ((1,height-1), Segment.Neither.trunk.mkTile(Color.Black, barkColor))
    )
    val segments = (1 until (height - 1)).map { k =>
      val typ = Segment.All.randomR(r)
      val left = typ.hasLeft.select(Seq(), Seq(leftLeaf))
      val right = typ.hasRight.select(Seq(), Seq(rightLeaf))
      val mid = typ.trunk.mkTile(Color.Black, barkColor).seq
      val segment: Vector[(Cell,Tile)] = Vector(
        left.map  { (l:Tile) => ((0, k), l)},
        mid.map   { (l:Tile) => ((1, k), l)},
        right.map { (l:Tile) => ((2, k), l)}


      ).flatten
      segment
    }.flatten
    val tree = segments ++ top ++ trunk
    Tree(xy, height, tree |++| xy)
  }
}

case class Tree private (xy:Cell, height:Int, tg:TileGroup) {
  def pos = xy
  def update = this
  def draw(tr:TileRenderer):TileRenderer = {
    tr <|| (tg |++| ((0, -height + 1)))
  }
  def getLights = Seq()
  def toDoodad:Doodad = Doodad[Tree](_.update, _.draw, _.getLights, _.pos, this)
}
