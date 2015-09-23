package de.flwi.fpInScala

import scala.annotation.tailrec


object Ex3_List {

  sealed trait List[+A] // `List` data type, parameterized on a type, `A`
  case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
  /* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
  which may be `Nil` or another `Cons`.
   */
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
      case Nil => 0 // The sum of the empty list is 0.
      case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
    }

    def product(ds: List[Double]): Double = ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x,xs) => x * product(xs)
    }

    def apply[A](as: A*): List[A] = // Variadic function syntax
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

    val x = List(1,2,3,4,5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y // <-- answer to 3.1
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }

    def append[A](a1: List[A], a2: List[A]): List[A] =
      a1 match {
        case Nil => a2
        case Cons(h,t) => Cons(h, append(t, a2))
      }

    def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
      as match {
        case Nil => z
        case Cons(x, xs) => f(x, foldRight(xs, z)(f))
      }

    def sum2(ns: List[Int]) =
      foldRight(ns, 0)((x,y) => x + y)

    def product2(ns: List[Double]) =
      foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


    def tail[A](l: List[A]): List[A] = l match {
      case Nil => sys.error("tail called on empty list")
      case Cons(_, t) => t
    }

    def setHead[A](l: List[A], h: A): List[A] = l match {
      case Nil => sys.error("setHead called on empty list")
      case Cons(_, tail) => Cons(h, tail)
    }

    @tailrec
    def drop[A](l: List[A], n: Int): List[A] = {

      if(n == 0) l
      else l match {
        case Nil => Nil
        case Cons(_, t) => drop(t, n-1)
      }
    }

    /**
     * needs to be curried to make type inference on f possible since the scala compiler doesn't provide _complete_ inference
     */
    def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
      case Nil => Nil
      case li @Cons(h, tail) if !f(h) => li
      case Cons(h, tail) if f(h) => dropWhile(tail)(f)
    }

    /**
     * returning a List consisting of all but the last element of a List
     */
    def init[A](l: List[A]): List[A] = {

      l match {
        case Nil => sys.error("init on empty list")
        case Cons(_, Nil) => Nil
        case Cons(h, tail) => Cons(h, init(tail))
      }
    }

    /**
     * returning a List consisting of all but the last element of a List
     */
    def initTailrec[A](l: List[A]): List[A] = {
      import collection.mutable.ListBuffer
      val buf = new ListBuffer[A]

      @annotation.tailrec
      def helper(current: List[A]): List[A] = {
        current match {
          case Nil => sys.error("init on empty list")
          case Cons(_, Nil) => List(buf.toList: _*)
          case Cons(h, tail) =>
            buf.append(h)
            helper(tail)
        }
      }

      helper(l)
    }

    def length[A](l: List[A]): Int = foldRight(l, 0)((_, cur) => cur + 1)

    @tailrec
    def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = {

      l match {
        case Nil => sys.error("foldLeft on empty list")
        case Cons(h, Nil) => f(z,h)
        case Cons(h, t) => foldLeft(t, f(z, h))(f)
      }
    }

    def leftFoldedSum(ints: List[Int]): Int = foldLeft(ints, 0)(_ + _)

    def leftFoldedProduct(ds: List[Double]): Double = foldLeft(ds, 1.0)(_ * _)

    def leftFoldedLength[A](l: List[A]): Int = foldLeft(l, 0)((cur, _) => cur + 1)

    def reverse[A](l: List[A]): List[A] = {
      foldLeft(l, Nil: List[A])((acc, curr) => Cons(curr, acc))
    }

    def rightFoldedAppend[A](a1: List[A], a2: List[A]): List[A] = {
      /*
        def append[A](a1: List[A], a2: List[A]): List[A] =
          a1 match {
            case Nil => a2
            case Cons(h,t) => Cons(h, append(t, a2))
          }
       */
      foldRight(a1, a2)(Cons(_,_))
    }

    def concatenate[A](lists: List[List[A]]): List[A] = {
      foldRight(lists, Nil: List[A])(append)
    }

    def add1toEachInt(ints: List[Int]): List[Int] = {
      foldRight(ints, Nil: List[Int])((cur,acc) => Cons(cur + 1, acc))
    }

    def toStringEachDouble(doubles: List[Double]): List[String] = {
      foldRight(doubles, Nil: List[String])((cur,acc) => Cons(cur.toString, acc))
    }

    def map[A,B](l: List[A])(f: A => B): List[B] = {
      foldRight(l, Nil: List[B])((cur, acc) => Cons(f(cur), acc))
    }

    def filter[A](as: List[A])(f: A => Boolean): List[A] = {
      foldRight(as, Nil: List[A])((cur, acc) => if(f(cur)) Cons(cur, acc) else acc)
    }

    def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {
      concatenate(map(as)(f))
    }

    def flatMappedFilter[A](as: List[A])(f: A => Boolean): List[A] = {
      flatMap(as)(a => if(f(a)) Cons(a, Nil) else Nil)
    }

    def addingElementsOfTwoLists(first: List[Int], second: List[Int]): List[Int] = {

      @tailrec
      def helper(l1: List[Int], l2: List[Int], result: List[Int]): List[Int] = {
        (l1, l2) match {
          case (Nil, Nil) => reverse(result)
          case (Nil, _) => Nil
          case (_, Nil) => Nil
          case (Cons(h1,t1), Cons(h2,t2)) => helper(t1,t2, Cons(h1 + h2, result))
        }
      }
      helper(first, second, Nil)
    }

    def zipWith[A, B, C](first: List[A], second: List[B])(f: (A,B) => C) = {

      @tailrec
      def helper(l1: List[A], l2: List[B], result: List[C]): List[C] = {
        (l1, l2) match {
          case (Nil, Nil) => reverse(result)
          case (Nil, _) => Nil
          case (_, Nil) => Nil
          case (Cons(h1,t1), Cons(h2,t2)) => helper(t1,t2, Cons(f(h1,h2), result))
        }
      }
      helper(first, second, Nil)
    }

    def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {

      @tailrec
      def helper(l: List[A], s: List[A]): Boolean = {
        (l, s) match {
          case (_, Nil) => true
          case(Nil, _) => false
          case(Cons(hL, tL), Cons(hS, _)) if hL != hS => helper(tL, s)
          case(Cons(hL, tL), Cons(hS, tS)) => helper(tL, tS)
        }
      }

      helper(sup, sub)
    }
  }
}
