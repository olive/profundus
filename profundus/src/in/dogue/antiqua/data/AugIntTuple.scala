package in.dogue.antiqua.data

class AugIntTuple(tup:(Int,Int)) {
  def range:Seq[(Int,Int)] = for (i <- 0 until tup._1; j <- 0 until tup._2) yield(i, j)
}
