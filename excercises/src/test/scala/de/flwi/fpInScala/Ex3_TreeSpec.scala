package de.flwi.fpInScala

import org.scalatest.{Matchers, FlatSpec}

class Ex3_TreeSpec extends FlatSpec with Matchers {

  import Ex3._

  "A tree" should "calculate the size correctly" in {
    Leaf(1).size shouldBe 1
    Branch(Leaf(1), Leaf(2)).size shouldBe 2
    Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)).size shouldBe 3
  }

  "A Tree[Int]" should "find the max value" in {
    Tree.maximum(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))) shouldBe 3
    Tree.maximum(Branch(Branch(Leaf(-1), Leaf(-2)), Leaf(-3))) shouldBe -1
  }
}
