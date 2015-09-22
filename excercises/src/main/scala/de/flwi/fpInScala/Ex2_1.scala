package de.flwi.fpInScala

object Ex2_1 extends App {

  def fibonacci(n: Int): Int = {
    if (n == 1 || n == 2) 1
    else if (n == 0) 0
    else fibonacci(n - 1) + fibonacci(n - 2)
  }


}