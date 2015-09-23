package de.flwi.fpInScala.Ex3

sealed trait Tree[+A] {
  def size: Int


}

case class Leaf[A](value: A) extends Tree[A] {
  override def size = 1
}

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A] {
  override def size = {
    left.size + right.size
  }
}


object Tree {


  def maximum[A: Numeric](tree: Tree[A]): A = {

    val numeric: Numeric[A] = implicitly[Numeric[A]]

    def helper(t: Tree[A], currentMax: A): A = {
      t match {
        case Leaf(value) =>
          numeric.max(value, currentMax)
        case Branch(left, right) =>
          val leftMax = helper(left, currentMax)
          val rightMax = helper(right, currentMax)
          numeric.max(leftMax, rightMax)
      }
    }

    helper(tree, numeric.fromInt(Int.MinValue))
  }

}