package in.dogue.profundus.world

import scala.util.Random
import in.dogue.antiqua.graphics.TileRenderer
import in.dogue.profundus.entities.EntityManager

object World {
  def create(cols:Int, rows:Int, r:Random) = {
    val terrain = TerrainCache.create(cols, rows, r)
    val es = EntityManager.create
    World(cols, rows, es, terrain)
  }
}

case class World(cols:Int, rows:Int, es:EntityManager, cache:TerrainCache) {

  def update(ppos:(Int,Int)) = {
    val (updates, particles, newEs) = es.update
    copy(cache=cache.checkPositions(ppos),
         es = newEs)
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
