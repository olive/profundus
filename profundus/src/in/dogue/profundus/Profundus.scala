package in.dogue.profundus

import in.dogue.antiqua.graphics.{Border, TextFactory, Tile}
import in.dogue.profundus.particles.{Emitter, Particle}
import in.dogue.profundus.deformations.Deformation
import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.data.CP437
import sun.io.CharToByteCp437
import in.dogue.profundus.world._
import in.dogue.profundus.entities.damagezones.DamageZone
import in.dogue.profundus.entities.pickups.Pickup
import in.dogue.profundus.entities.{GameBox, Entity, Obelisk, Lurker}
import in.dogue.profundus.world.PickupSpawn
import in.dogue.antiqua.graphics.Border
import in.dogue.profundus.world.NewParticles
import in.dogue.profundus.world.NewDeformations
import in.dogue.profundus.world.NewDamageZones
import in.dogue.profundus.deformations.Deformation
import in.dogue.profundus.doodads.Doodad
import in.dogue.antiqua.algebra.Monoid
import in.dogue.profundus.ui.MessageBox

object Profundus {
  class AugNewParticles(s:Seq[Particle]) {
    def gs = NewParticles(s)
    def gss = Seq(gs)
  }
  implicit def particles2NewSpawn(s:Seq[Particle]) = new AugNewParticles(s)

  class AugNewDamageZones(s:Seq[DamageZone]) {
    def gs = NewDamageZones(s)
    def gss = Seq(gs)
  }
  implicit def damageZones2NewSpawn(s:Seq[DamageZone]) = new AugNewDamageZones(s)

  class AugNewMessageBox(s:GameBox) {
    def gs = NewMessageBox(s)
    def gss = Seq(gs)
  }
  implicit def damageZones2NewMessageBox(s:GameBox) = new AugNewMessageBox(s)


  class AugNewDeformations(s:Seq[Deformation]) {
    def gs = NewDeformations(s)
    def gss = Seq(gs)
  }
  implicit def deformations2NewSpawn(s:Seq[Deformation]) = new AugNewDeformations(s)
  class AugNewEmitters(s:Seq[Emitter]) {
    def gs = NewEmitters(s)
    def gss = Seq(gs)
  }
  implicit def deformations2NewEmitters(s:Seq[Emitter]) = new AugNewEmitters(s)

  class AugPickupSpawn(s:Seq[Pickup]) {
    def ws = PickupSpawn(s)
    def wss = Seq(ws)
  }
  implicit def pickup2PickupSpawn(s:Seq[Pickup])  = new AugPickupSpawn(s)

  class AugEntitySpawn(s:Seq[Entity[_]]) {
    def ws = EntitySpawn(s)
    def wss = Seq(ws)
  }
  implicit def creature2EntitySpawn(s:Seq[Entity[_]]) = new AugEntitySpawn(s)

  val tf = TextFactory(Color.Black, Color.White, CP437.unicodeToCode)
  val border = Border(CP437.doubleBorder)(Color.Black, Color.White) _



  import Monoid._
  def fold2[A,B,C](init:A, as:Seq[C])(f:(C,A) => (A,B))(implicit ev:Monoid[B]):(A,B) = {
    val seed = (init, ev.zero)
    as.foldLeft(seed) { case ((a, b), c) =>
      val (ap, bp) = f(c, a)
      (ap, b <+> bp)
    }
  }

  def fold3[A, B1, B2, C](init:A, as:Seq[C])(f:(C, A) => (A, B1, B2))(implicit ev:Monoid[B1], ev2:Monoid[B2]):(A,B1, B2) = {
    val seed = (init, ev.zero, ev2.zero)
    as.foldLeft(seed) { case ((a, b1, b2), c) =>
      val (ap, b1p, b2p) = f(c, a)
      (ap, b1 <+> b1p, b2 <+> b2p)
    }
  }
}
