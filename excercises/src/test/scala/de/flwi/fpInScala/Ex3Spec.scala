package de.flwi.fpInScala

import org.scalatest._

class Ex3Spec extends FlatSpec with Matchers {

  import Ex3_List._

  "List" should "return the same if 0 elements are being dropped" in {
    List.drop(List(1, 2, 3), 0) shouldBe List(1, 2, 3)
  }
  it should "drop 1 element correctly" in {
    List.drop(List(1, 2, 3), 1) shouldBe List(2, 3)
  }

  it should "dropWhile correctly" in {
    List.dropWhile(List(1, 2, 3))(_ < 2) shouldBe List(2, 3)
    List.dropWhile(List(1, 2, 3))(_ < 1) shouldBe List(1, 2, 3)
    List.dropWhile(List(1, 2, 3))(_ < 5) shouldBe Nil
  }

  it should "init correctly (returning a List consisting of all but the last element of a List)" in {
    List.init(List(1, 2, 3, 4)) shouldBe List(1, 2, 3)
  }

  it should "initTailrec correctly (returning a List consisting of all but the last element of a List)" in {
    List.initTailrec(List(1, 2, 3, 4)) shouldBe List(1, 2, 3)
  }

  it should "let me see what Exercise 3.8 does" in {
    val result = List.foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _))
    result shouldBe List(1, 2, 3)

    /*
    foldRight(Cons(1, Cons(2, Cons(3, Nil))), Nil:List[Int])(Cons(_,_))
    Cons(1, foldRight(Cons(2, Cons(3, Nil)), Nil:List[Int])(Cons(_,_)))
    Cons(1, Cons(2, foldRight(Cons(3, Nil), Nil:List[Int])(Cons(_,_))))
    Cons(1, Cons(2, Cons(3, foldRight(Nil, Nil:List[Int])(Cons(_,_)))))
    Cons(1, Cons(2, Cons(3, Nil)))
    */
  }

  it should "calculate the length correctly" in {
    List.length(List(1, 2, 3, 4)) shouldBe 4
    List.length(Nil) shouldBe 0
  }

  it should "behave the same with sum, product and length using the recursive and tailrecursive mplementations" in {
    Vector(1.to(4), 0.to(1), 1.to(1000)).foreach {
      range =>
        val ints = List(range: _*)
        val doubles = List(range.map(_.toDouble): _*)
        List.sum(ints) shouldBe List.leftFoldedSum(ints)
        List.product(doubles) shouldBe List.leftFoldedProduct(doubles)
        List.length(ints) shouldBe List.leftFoldedLength(ints)
    }
  }
  it should "reverse correctly" in {
    List.reverse(List(1, 2, 3, 4)) shouldBe List(4,3,2,1)
  }

  it should "append correctly" in {
    List.append(List(1,2), List(3,4)) shouldBe List(1,2,3,4)
    List.append(Nil, List(3,4)) shouldBe List(3,4)
    List.append(List(1,2), Nil) shouldBe List(1,2)
  }

  it should "behave the same with append using given and my impl" in {
    List.append(List(1,2,3), List(4,5,6)) shouldBe List.rightFoldedAppend(List(1,2,3), List(4,5,6))
  }

  it should "concatenate Lists correctly" in {
    List.concatenate(List(List(1,2), List(3,4), List(5,6))) shouldBe List(1,2,3,4,5,6)
  }
}