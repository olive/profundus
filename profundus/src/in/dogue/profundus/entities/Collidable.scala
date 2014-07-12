package in.dogue.profundus.entities

case class Collidable[T](pos: T => (Int,Int),
                         intersects: T => (Collidable[P] forSome {type P}) => Boolean,
                         onCollide: T => (Collidable[P] forSome {type P}) => T,
                         self:T)  {


}
