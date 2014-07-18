package in.dogue.profundus.entities



object Attributes {
  def create = Attributes(5, 5, 0)
  val default = create
}
case class Attributes(stamRegen:Int, healthRegen:Int, toolRegen:Int) {

}
