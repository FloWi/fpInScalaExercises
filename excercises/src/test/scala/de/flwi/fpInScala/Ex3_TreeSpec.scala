package de.flwi.fpInScala

import org.scalatest.{Matchers, FlatSpec}

class Ex3_TreeSpec extends FlatSpec with Matchers {

  import Ex3._

  "A tree" should "calculate the size correctly" in {
    Leaf(1).size shouldBe 1
    Branch(Leaf(1), Leaf(2)).size shouldBe 2
    Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)).size shouldBe 3
  }

  it should "calculate the depth correctly" in {
    Leaf(1).depth shouldBe 0
    Branch(Leaf(1), Leaf(2)).depth shouldBe 1
    Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)).depth shouldBe 2
  }

  it should "map correctly" in {

    Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)).map(_.toString) shouldBe Branch(Branch(Leaf("1"), Leaf("2")), Leaf("3"))
    Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)).map(_*2) shouldBe Branch(Branch(Leaf(2), Leaf(4)), Leaf(6))
  }

  "A Tree[Int]" should "find the max value" in {
    Tree.maximum(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))) shouldBe 3
    Tree.maximum(Branch(Branch(Leaf(-1), Leaf(-2)), Leaf(-3))) shouldBe -1
  }
}
