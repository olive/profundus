package in.dogue.profundus.world

import com.deweyvm.gleany.data.Recti
import in.dogue.antiqua.Antiqua
import Antiqua._

object Unloadable {
  def fromPos[T](t:T, getPos:T => Cell) = new Unloadable[T] {
    override def shouldUnload(rect:Recti):Boolean = !rect.contains(getPos(t))
    override def get:T = t
  }
}

trait Unloadable[T] {
  def shouldUnload(recti:Recti):Boolean
  def get:T
}


class WorldFilter {
  def filter[T](r:Recti)(s:Seq[T], toUnloadable:T=>Unloadable[T]):Seq[T] = {
    s.map { t => t @@ toUnloadable(t)}.filter{!_._2.shouldUnload(r)}.map{_._1}

  }
}
