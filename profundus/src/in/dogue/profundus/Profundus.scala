package in.dogue.profundus

import in.dogue.antiqua.graphics.{Border, TextFactory, Tile}
import in.dogue.profundus.particles.{Emitter, Particle}
import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.data.CP437
import in.dogue.profundus.entities.damagezones.DamageZone
import in.dogue.profundus.entities.pickups.Pickup
import in.dogue.profundus.entities._
import in.dogue.profundus.deformations.Deformation
import in.dogue.profundus.doodads.Doodad
import in.dogue.antiqua.algebra.Monoid
import in.dogue.antiqua.graphics.Border
import in.dogue.profundus.world.Transaction
import in.dogue.profundus.world.DestroyEntity
import in.dogue.profundus.entities.GameBox
import in.dogue.profundus.world.NewDoodads
import in.dogue.profundus.world.NewEntities
import in.dogue.profundus.world.NewDamageZones
import in.dogue.profundus.world.NewMessageBox
import in.dogue.profundus.world.NewEmitters
import in.dogue.profundus.world.NewParticles
import in.dogue.profundus.world.NewDeformations
import in.dogue.profundus.world.NewTransaction
import in.dogue.profundus.world.NewPickups

object Profundus {
  class AugNewParticles(s:Seq[Particle]) {
    def gm = NewParticles(s)
    def gms = Seq(gm)
  }
  implicit def particles2NewSpawn(s:Seq[Particle]) = new AugNewParticles(s)

  class AugNewDamageZones(s:Seq[DamageZone]) {
    def gm = NewDamageZones(s)
    def gms = Seq(gm)
  }
  implicit def damageZones2NewSpawn(s:Seq[DamageZone]) = new AugNewDamageZones(s)

  class AugNewMessageBox(s:GameBox) {
    def gm = NewMessageBox(s)
    def gms = Seq(gm)
  }
  implicit def damageZones2NewMessageBox(s:GameBox) = new AugNewMessageBox(s)

  class AugNewDeformations(s:Seq[Deformation]) {
    def gm = NewDeformations(s)
    def gms = Seq(gm)
  }
  implicit def deformations2NewSpawn(s:Seq[Deformation]) = new AugNewDeformations(s)
  class AugNewEmitters(s:Seq[Emitter]) {
    def gm = NewEmitters(s)
    def gms = Seq(gm)
  }
  implicit def emitters2NewEmitters(s:Seq[Emitter]) = new AugNewEmitters(s)

  class AugNewPickups(s:Seq[Pickup]) {
    def gm = NewPickups(s)
    def gms = Seq(gm)
  }
  implicit def pickup2NewPickups(s:Seq[Pickup])  = new AugNewPickups(s)

  class AugNewEntitys(s:Seq[Entity]) {
    def gm = NewEntities(s)
    def gms = Seq(gm)
  }
  implicit def creature2NewEntitys(s:Seq[Entity]) = new AugNewEntitys(s)

  class AugNewDoodads(s:Seq[Doodad]) {
    def gm = NewDoodads(s)
    def gms = Seq(gm)
  }
  implicit def doodad2NewDoodads(s:Seq[Doodad]) = new AugNewDoodads(s)

  class AugNewTransaction(s:Transaction) {
    def gm = NewTransaction(s)
    def gms = Seq(gm)
  }
  implicit def transaction2NewTransaction(s:Transaction) = new AugNewTransaction(s)

  class AugDestroyEntity(s:EntityId) {
    def gm = DestroyEntity(s)
    def gms = Seq(gm)
  }
  implicit def transaction2DestroyEntity(s:EntityId) = new AugDestroyEntity(s)
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
