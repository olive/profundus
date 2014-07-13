package in.dogue.profundus.world

import scala.util.Random
import in.dogue.antiqua.graphics.TileRenderer
import in.dogue.profundus.entities.{Player, EntityManager}
import in.dogue.profundus.particles.Particle
import in.dogue.profundus.deformations.Deformation

object World {
  def create(cols:Int, rows:Int, r:Random):(World,(Int,Int)) = {
    val (terrain, spawn) = TerrainCache.create(cols, rows, r)
    val es = EntityManager.create
    val w = World(cols, rows, es, terrain, Seq())
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

case class World(cols:Int, rows:Int, es:EntityManager, cache:TerrainCache, ds:Seq[Deformation[_]]) {

  def update(ppos:(Int,Int)):(World, Seq[Particle[A] forSome {type A}]) = {
    val (updates, particles, newEs) = es.update(this)
    val gravEs = newEs.doGravity(this)
    val newCache = cache.checkPositions(ppos).update(ppos)
    val newWorld = copy(cache=newCache,
                        es=gravEs,
                        ds=ds++updates)
    (World.doDeformations(newWorld), particles)
  }

  def collectGems(p:Player):(World, Player) = {
    val (newP, newEs) = es.collectGems(p)
    (copy(es=newEs), newP)
  }

  def insertBomb(ij:(Int,Int)):World = {
    copy(es = es.spawnCapsule(ij))
  }

  def insertRope(ij:(Int,Int)):World = {
    copy(es = es.spawnRope(ij))
  }

  def isSolid(ij:(Int,Int)):Boolean = {
    cache.isSolid(ij)
  }

  def isRope(ij:(Int,Int)):Boolean = {
    es.ropes.exists{_.ropePos(ij)}
  }

  def isGrounded(ij:(Int,Int)):Boolean = {
    cache.isGrounded(ij)
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
