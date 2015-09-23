package de.flwi.fpInScala.ex2

object Ex2_3_to_5 {

  //example
  def partial1[A,B,C](a: A, f: (A,B) => C): B => C = {
    f(a, _)
    //shorthand for b => f(a, b)
    //shorthand for (b: B) => f(a, b)
  }

  //Exercise 2.3
  def curry[A,B,C](f: (A,B) => C): A => (B => C) = {
    //a => f(a, _) //my solution by accident ;-)

    //official solution
    //Note that since => associates to the right, A => (B => C) can be written as A => B => C.

    //IDEA offers action on b --> introduce implicit parameter, which leads to my upper solution
    a => b => f(a, b)
  }

  //Exercise 2.4
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
    (a, b) => f(a)(b)
  }

  //Exercise 2.5
  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    a => f(g(a))
    // to ocmpose two functions one could write
    // g andThen f would work as well as
    // f compose g since it is a common thing to do. so there is a method in the Function1 trait
  }

  def cos() = {
    val f = (x: Double) => math.Pi / 2 - x

    val cos = f andThen math.sin

    cos
  }
}
