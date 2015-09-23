package de.flwi.fpInScala.Ex3

sealed trait Tree[+A] {
  def size: Int

  def depth: Int


}

case class Leaf[A](value: A) extends Tree[A] {
  override def size = 1
  override def depth = 0
}

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A] {

  override def size = left.size + right.size
  override def depth = 1 + (left.depth max right.depth)
}


object Tree {

  def maximum[A](tree: Tree[A])(implicit n: Numeric[A]): A = {
    tree match {
      case Leaf(value) => value
      case Branch(left, right) => n.max(maximum(left), maximum(right))
    }
  }
}