package in.dogue.profundus.world

import com.deweyvm.gleany.data.Recti
import scala.util.Random

case class Feature(private val rect:Recti, private val f:Terrain => Random => Terrain) {
  def intersects(other:Feature) = other.rect.intersects(rect)
  def transform(t:Terrain, r:Random):Terrain = f(t)(r)
}
