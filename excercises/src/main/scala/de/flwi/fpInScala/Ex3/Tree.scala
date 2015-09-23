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

  def maximum[A](tree: Tree[A])(implicit n: Numeric[A]): A = {
    tree match {
      case Leaf(value) => value
      case Branch(left, right) => n.max(maximum(left), maximum(right))
    }
  }
}