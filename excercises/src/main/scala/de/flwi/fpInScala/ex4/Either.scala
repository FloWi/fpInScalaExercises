package de.flwi.fpInScala.ex4

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.{Either => _, Left => _, Option => _, Right => _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E,+A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => Right(f(a))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => f(a)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => b
    case Right(a) => Right(a)
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    for {
      aa <- this
      bb <- b
    } yield f(aa,bb)
  }
}
case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Either {
  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    val buf = ArrayBuffer.empty[B]

    @tailrec
    def helper(list: List[A]): Either[E, List[B]] = list match {
      case Nil => Right(buf.toList)
      case h :: cons => f(h) match {
        case Left(e) => Left(e)
        case Right(a) => buf += a; helper(cons)
      }
    }

    es match {
      case Nil => sys.error("called sequence on empty list")
      case _ => helper(es)
    }
  }

  def traverseFromSolution[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es match {
      case Nil => Right(Nil)
      case h::t =>
        //funktion anwenden
        //mit map2 auf Either[E, B] und Either[E, List[B]] die b :: bs funktion anwenden
        (f(h) map2 traverseFromSolution(t)(f))((b, bs) => b :: bs)
    }

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse(es)(e => e)

  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

}