package de.flwi.fpInScala.ex2

import scala.annotation.tailrec

object Ex2_1 extends App {

  def fibonacci(n: Int): Int = {
    if (n == 1 || n == 2) 1
    else if (n == 0) 0
    else fibonacci(n - 1) + fibonacci(n - 2)
  }

  /**
   * by http://peter-braun.org/2012/06/fibonacci-numbers-in-scala/
   */
  def fibonacciTailRec(n: Int): Int = {

    @tailrec
    def fib_tail(n: Int, a: Int, b: Int): Int = n match {
      case 0 => a
      case _ => fib_tail(n - 1, b, a + b)
    }
    fib_tail(n, 0, 1)
  }
}