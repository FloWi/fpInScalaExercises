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


}