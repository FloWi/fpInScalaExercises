package de.flwi.fpInScala.ex5

import Stream._

import scala.annotation.tailrec

trait Stream[+A] {

  /**
   * f takes the 2nd argument by name and my choose not to evaluate it
   */
  def foldRight[B](z: => B)(f: (A, => B) => B): B = {
    // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }
  }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList: List[A] = {

    def helper(cur: Stream[A], res: List[A]): List[A] = cur match {
      case Cons(a, cons) => helper(cons(), a() :: res)
      case Empty => res.reverse
    }

    helper(this, Nil)
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  def takeViaUnfold(n: Int): Stream[A] = unfold((n, this)){
      case (0, _) => None
      case (remainder, Empty) => None
      case (remainder, Cons(h, t)) => Some(h(), (remainder-1, t()))
    }

  @tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n-1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case Cons(h, _) if !p(h()) => empty
    case _ => this
  }

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((value, b) => if(p(value)) cons(value, b) else empty)

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = unfold(this){
      case Cons(h,_) if !p(h())=> None
      case Empty => None
      case Cons(h,t) => Some(h(), t())
    }

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((value, b) => p(value) && b)

  def headOption: Option[A] = {
    foldRight(None: Option[A])((a, _) => Some(a))
  }

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a, acc) => cons(f(a), acc))

  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold[B, Stream[A]](this){
      case Cons(h, t) => Some(f(h()), t())
      case Empty => None
    }

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a,acc) => if(p(a)) cons(a, acc) else acc)

  def append[B >: A](other: => Stream[B]): Stream[B] =
    foldRight(other)((h,t) => cons(h,t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a, acc) => f(a) append acc)

  def zipWith[B, C](other: Stream[B])(f: (A,B) => C): Stream[C] = unfold((this, other)){
    case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
    case _ => None
  }

  def zipAll[B](other: Stream[B]): Stream[(Option[A],Option[B])] = unfold((this, other)){
    case (Empty, Empty) => None
    case (Empty, Cons(h2, t2)) => Some((None, Some(h2())), (empty, t2()))
    case (Cons(h1, t1), Empty) => Some((Some(h1()), None), (t1(), empty))
    case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
  }

  def startsWith[A](s: Stream[A]): Boolean =
    zipAll(s).forAll {
      case (Some(a), Some(b)) => a == b
      case (Some(a), None) => true
      case _ => false
    }

  def startsWithFromSolution[A](s: Stream[A]): Boolean =
    zipAll(s).takeWhile(_._2.isDefined) forAll {
      case (h,h2) => h == h2
    }

  def tails: Stream[Stream[A]] = unfold(this) {
    case s@Cons(h, t) => Some((s, t()))
    case _ => None
  } append Stream(empty)

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails exists (_ startsWith s)

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    //TODO: I would never have come up with s.th. like that myself
    foldRight((z, Stream(z)))((a, p0) => {
      // p0 is passed by-name and used in by-name args in f and cons. So use lazy val to ensure only one evaluation...
      lazy val (b, stream) = p0
      val b2 = f(a, b)
      (b2, cons(b2, stream))
    })._2

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  val onesViaUnfold: Stream[Int] = constantViaUnfold(1)

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def constantViaUnfold[A](a: A): Stream[A] = unfold(a)(_ => Some(a, a))

  def from(n: Int): Stream[Int] = cons(n, from(n+1))

  def fromViaUnfold(n: Int): Stream[Int] = unfold(n)(s => Some(s, s+1))

  def fibs: Stream[Int] = {

    def helper(a: Int, b: Int): Stream[Int] = {
      cons(a, helper(b, a+b))
    }
    helper(0, 1)
  }

  def fibsViaUnfold: Stream[Int] =
    unfold[Int, (Int, Int)]((0,1)){
      case(a, b) => Some(a, (b, a+b))
    }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {

    def helper(cur: S): Stream[A] = {
      f(cur) match {
        case None => empty
        case Some((a, s)) => cons(a, helper(s))
      }
    }

    helper(z)
  }
}