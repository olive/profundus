package in.dogue.profundus.world

import scala.util.Random
import in.dogue.antiqua.graphics.TileRenderer
import in.dogue.profundus.entities.{KillZone, Player, EntityManager}
import in.dogue.profundus.particles.Particle
import in.dogue.profundus.deformations.Deformation
import in.dogue.antiqua.data.Direction

object World {
  def create(cols:Int, rows:Int, r:Random):(World,(Int,Int)) = {
    val (terrain, spawn) = TerrainCache.create(cols, rows, r)
    val es = EntityManager.create(r)
    val w = World(cols, rows, es, terrain, Seq(), Seq())
    (w, spawn)
  }

  private def doDeformations(world:World):World = {
    val ds = world.ds
    val deformed = ds.foldLeft(world){case (w, d) =>
      d.apply(w)
    }
    val newDs = ds.map{_.update}.flatten
    deformed.copy(ds = newDs)
  }
}

case class World(cols:Int, rows:Int, es:EntityManager, cache:TerrainCache, ds:Seq[Deformation[_]], kz:Seq[KillZone[_]]) {

  def update(ppos:(Int,Int)):(World, Seq[Particle[A] forSome {type A}]) = {
    val newCache = cache.checkPositions(ppos).update(ppos)
    val newW = this.copy(cache=newCache)
    val (updates, kills, particles, newEs) = es.update(newW)
    val newKz = kz.map{_.update}.flatten
    val gravEs = newEs.doGravity(newW)
    val creaEs = gravEs.updateCreatures(newCache, ppos)
    val newWorld = newW.copy(es=creaEs,
                             ds=ds++updates,
                             kz = newKz ++ kills)
    (World.doDeformations(newWorld), particles)
  }

  def interact(p:Player):(Player, EntityManager) = {
    es.interact(p)
  }

  def killEntities(p:Player):(EntityManager, Player, Seq[Particle[A] forSome {type A}]) = {
    val pl = if (kz.exists{ _.contains(p.pos)}) {
      p.kill
    } else{
      p
    }
    val (newEs, ps) = es.doKill(kz)
    (newEs, pl, ps)
  }

  def collectGems(p:Player):(World, Player) = {
    val (newP, newEs) = es.collectGems(p)
    (copy(es=newEs), newP)
  }

  def insertBomb(ij:(Int,Int)):World = {
    copy(es = es.spawnCapsule(ij))
  }

  def insertRope(ij:(Int,Int), d:Direction):World = {
    copy(es = es.spawnRope(ij, d))
  }

  def isSolid(ij:(Int,Int)):Boolean = {
    cache.isSolid(ij) || es.existsSolid(ij)
  }

  def isRope(ij:(Int,Int)):Boolean = {
    es.ropes.exists{_.ropeContains(ij)}
  }

  def isGrounded(ij:(Int,Int)):Boolean = {
    cache.isGrounded(ij)
  }

  def onScreen(ij:(Int,Int)):Boolean = {
    cache.isLoaded(ij)
  }

  def hit(ij:(Int,Int)):(World, Int) = {
    val (newCache, dropped, damage) = cache.hit(ij)
    val newWorld = copy(cache=newCache, es=es.addDrops(dropped))
    (newWorld, damage)
  }

  def draw(pl:(Int,Int))(tr:TileRenderer):TileRenderer = {
    tr <+< cache.draw(pl) <+< es.draw
  }

}
