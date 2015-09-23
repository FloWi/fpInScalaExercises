package de.flwi.fpInScala.ex2

import org.scalatest._



class Ex2_2Spec extends FlatSpec with Matchers {

  "Ex2_2.isSorted" should "detect sorted array correctly" in {
    Ex2_2.isSorted(Array(1, 2, 3, 4), (a: Int, b: Int) => a <= b) shouldBe true
  }
  it should "detect unsorted array correctly" in {
    Ex2_2.isSorted(Array(2, 1, 3, 4), (a: Int, b: Int) => a <= b) shouldBe false
  }
  it should "mark an empty array as sorted" in {
    Ex2_2.isSorted(Array.empty, (a: Int, b: Int) => a <= b) shouldBe true
  }
}