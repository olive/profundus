package in.dogue.profundus.world

import scala.util.Random
import in.dogue.antiqua.graphics.TileRenderer
import in.dogue.profundus.entities._
import in.dogue.profundus.particles.Particle
import in.dogue.profundus.deformations.Deformation
import in.dogue.antiqua.data.Direction
import in.dogue.profundus.particles.Particle
import in.dogue.profundus.deformations.Deformation
import in.dogue.profundus.entities.KillZone

object World {
  def create(cols:Int, rows:Int, r:Random):(World,(Int,Int)) = {
    val (terrain, spawn) = TerrainCache.create(cols, rows, r)
    val es = EntityManager.create(r)
    val w = World(cols, rows, es, terrain, Seq(), Seq())
    (w, spawn)
  }

  private def doDeformations(world:World):World = {
    val ds = world.ds
    val seed = (world.cache, Seq[MineralDrop](), 0)
    val (deformed, mins, _) = ds.foldLeft(seed){case ((tc, mins, dmg), d) =>
      val (nc, drop, hit) = d.apply(tc)
      (nc, drop ++ mins, dmg+hit)
    }
    val newDs = ds.map{_.update}.flatten
    world.copy(ds = newDs, cache=deformed, es=world.es.addDrops(mins))
  }
}

case class World(cols:Int, rows:Int, es:EntityManager, cache:TerrainCache, ds:Seq[Deformation[_]], kz:Seq[KillZone[_]]) {

  def update(ppos:(Int,Int), pState:LivingState):(World, Seq[Particle[A] forSome {type A}]) = {
    val newCache = cache.checkPositions(ppos).update(ppos)
    val newW = this.copy(cache=newCache)
    val (updates, kills, particles, newEs) = es.update(newW.cache)
    val newKz = kz.map{_.update}.flatten
    val gravEs = newEs.doGravity(newW.cache)
    val (creaEs, cKills) = gravEs.updateCreatures(newCache, ppos, pState)
    val newWorld = newW.copy(es=creaEs,
                             ds=ds++updates,
                             kz = newKz ++ kills ++ cKills)
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

  /*def hit(ij:(Int,Int)):(World, Int) = {
    val (newCache, dropped, damage) = cache.hit(ij)
    val newWorld = copy(cache=newCache, es=es.addDrops(dropped))
    (newWorld, damage)
  }*/

  def draw(pl:(Int,Int))(tr:TileRenderer):TileRenderer = {
    tr <+< cache.draw(pl) <+< es.draw
  }

}
