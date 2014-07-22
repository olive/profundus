package in.dogue.profundus.mode

import in.dogue.antiqua.graphics.TileRenderer
import in.dogue.profundus.input.Controls
import in.dogue.profundus.{Game, Profundus}


case class Mode[T](up:T => Mode[_],
                   dr:T => (TileRenderer) => TileRenderer,
                   self:T) {
  def update:Mode[_] = {
    up(self)
  }
  def draw(tr:TileRenderer):TileRenderer = {
    val drawn = tr <+< dr(self)
    if (Controls.Debug.isPressed) {
      val tf = Profundus.tf
      val longest = Vector(Game.updatePerf.getLongest, Game.globPerf.getLongest, Game.drawPerf.getLongest).max
      val lines = Game.updatePerf.print(longest).map(tf.create)
      val lines2 = Game.globPerf.print(longest).map(tf.create)
      val lines3 = Game.drawPerf.print(longest).map(tf.create)
      drawn <++< lines.zipWithIndex.map{ case (l, k) =>
        l.draw((0, k)) _
      } <++< lines2.zipWithIndex.map { case (l, k) =>
        l.draw((0,k + lines.length + 2)) _
      } <++< lines3.zipWithIndex.map { case (l, k) =>
        l.draw((0,k + lines.length + lines2.length + 4)) _
      }
    } else {
      drawn
    }
  }
}
