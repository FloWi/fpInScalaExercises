package de.flwi.fpInScala

import org.scalatest._

class Ex3Spec extends FlatSpec with Matchers {

  import Ex3_List._

  "List" should "return the same if 0 elements are being dropped" in {
    List.drop(List(1, 2, 3), 0) shouldBe List(1, 2, 3)
  }
  it should "drop 1 element correctly" in {
    List.drop(List(1,2,3), 1) shouldBe List(2,3)
  }
  it should "dropWhile correctly" in {
    List.dropWhile(List(1,2,3), (_: Int) < 2) shouldBe List(2,3)
    List.dropWhile(List(1,2,3), (_: Int) < 1) shouldBe List(1,2,3)
    List.dropWhile(List(1,2,3), (_: Int) < 5) shouldBe Nil
  }
}