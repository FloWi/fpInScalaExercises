package de.flwi.fpInScala.ex2

import org.scalatest._

class Ex2_1Spec extends FlatSpec with Matchers {
  "Ex2_1.fibonacci" should "work correctly with the first 15 elements (wikipedia veryfied)" in {
    import Ex2_1.fibonacci
    Stream
      .from(0)
      .map(fibonacci)
      .take(15)
      .toList shouldBe List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377)
  }

  "Ex2_1.fibonacciTailRec" should "get exactly the same results" in {
    import Ex2_1._
    Stream
      .from(0)
      .map(n => (fibonacci(n), fibonacciTailRec(n)))
      .take(30)
      .toList
      .forall(t => t._1 == t._2) shouldBe true
  }

  it should "run reasonably fast" in {
    import Ex2_1._

    val start = System.currentTimeMillis()
    fibonacciTailRec(1000)
    val end = System.currentTimeMillis()

    (end - start) should be < 1000L
  }
}
