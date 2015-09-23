package de.flwi.fpInScala.Ex3

sealed trait Tree[+A] {
  def size: Int = {
    1
  }

}

case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {


}