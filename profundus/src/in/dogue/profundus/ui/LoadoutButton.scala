package in.dogue.profundus.ui

import in.dogue.antiqua.graphics.TileRenderer
import in.dogue.profundus.mode.loadout.{LoadoutUpdate, LoadoutMode, Loadout}
import in.dogue.profundus.mode.Mode

case class LoadoutButton[T](up:T => (Mode[_], Int) => (LoadoutUpdate, T,Int),
                            fill : T => Loadout => Loadout,
                            dr:T => (Boolean,Int) =>TileRenderer => TileRenderer,
                            self:T) {
  def update(m:Mode[_], rem:Int) = {
    val (newMode, newSelf, newRem) = up(self)(m, rem)
    (newMode, copy(self=newSelf), newRem)
  }

  def doFill(lo:Loadout) = fill(self)(lo)

  def draw(selected:Boolean, rem:Int)(tr:TileRenderer):TileRenderer = {
    tr <+< dr(self)(selected, rem)
  }
}
