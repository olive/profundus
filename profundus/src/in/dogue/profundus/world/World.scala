package in.dogue.profundus.world

import scala.util.Random
import in.dogue.antiqua.graphics.TileRenderer
import in.dogue.profundus.entities.EntityManager
import in.dogue.profundus.particles.Particle
import in.dogue.profundus.deformations.Deformation

object World {
  def create(cols:Int, rows:Int, r:Random) = {
    val terrain = TerrainCache.create(cols, rows, r)
    val es = EntityManager.create
    World(cols, rows, es, terrain, Seq())
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
    val (updates, particles, newEs) = es.update
    val gravEs = newEs.doGravity(this)
    if (updates.length > 0) {
      println("got one")
    }

    val newCache = cache.checkPositions(ppos)
    val newWorld = copy(cache=newCache,
                        es=gravEs,
                        ds=ds++updates)
    (World.doDeformations(newWorld), particles)
  }


  def insertBomb(ij:(Int,Int)):World = {
    copy(es = es.spawnCapsule(ij))
  }

  def isSolid(ij:(Int,Int)):Boolean = {
    cache.isSolid(ij)
  }

  def isGrounded(ij:(Int,Int)):Boolean = {
    cache.isGrounded(ij)
  }

  def break(ij:(Int,Int)):World = {
    copy(cache=cache.break(ij))
  }

  def draw(pl:(Int,Int))(tr:TileRenderer):TileRenderer = {
    tr <+< cache.draw(pl) <+< es.draw
  }

}
