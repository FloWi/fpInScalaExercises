package de.flwi.fpInScala.ex3

sealed trait Tree[+A] {
  def size: Int

  def depth: Int

  def map[B](f: (A) => B): Tree[B] = {

    def helper(tree: Tree[A]): Tree[B] = {

      tree match {
        case Leaf(value) => Leaf(f(value))
        case Branch(left, right) => Branch(helper(left), helper(right))
      }
    }
    helper(this)
  }

  def fold[B](l: A => B)(b: (B,B) => B): B = {

    def helper(t: Tree[A]): B = t match {
      case Leaf(value) => l(value)
      case Branch(left, right) => b(helper(left), helper(right))
    }

    helper(this)
  }

  def foldedSize: Int = fold(_ => 1)(1+_+_)
  def foldedDepth: Int = fold(_ => 0)((left, right) => 1 + left max right)
  def foldedMap[B](f: (A) => B): Tree[B] = fold(a => Leaf(f(a)): Tree[B])(Branch(_,_))
}

case class Leaf[A](value: A) extends Tree[A] {
  override def size = 1
  override def depth = 0
}

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A] {

  override def size = 1 + left.size + right.size
  override def depth = 1 + (left.depth max right.depth)
}


object Tree {

  def maximum[A](tree: Tree[A])(implicit n: Numeric[A]): A = {
    tree match {
      case Leaf(value) => value
      case Branch(left, right) => n.max(maximum(left), maximum(right))
    }
  }

  def foldedMaximum[A](tree: Tree[A])(implicit n: Numeric[A]): A = tree.fold(a => a)(n.max)

}