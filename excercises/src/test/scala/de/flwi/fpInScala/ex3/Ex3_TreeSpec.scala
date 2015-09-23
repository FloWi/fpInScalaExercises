package de.flwi.fpInScala.ex3

import de.flwi.fpInScala.ex3
import org.scalatest.{FlatSpec, Matchers}

class Ex3_TreeSpec extends FlatSpec with Matchers {

  import ex3._

  "A tree" should "calculate the size correctly" in {
    Leaf(1).size shouldBe 1
    Branch(Leaf(1), Leaf(2)).size shouldBe 3
    Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)).size shouldBe 5
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

  /*
  Generalize size, maximum, depth, and map, writing a new function fold that abstracts over their similarities.
  Reimplement them in terms of this more general function.
 */
  it should "calculate the foldedSize correctly" in {
    Leaf(1).foldedSize shouldBe 1
    Branch(Leaf(1), Leaf(2)).foldedSize shouldBe 3
    Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)).foldedSize shouldBe 5
  }

  it should "calculate the foldedDepth correctly" in {
    Leaf(1).foldedDepth shouldBe 0
    Branch(Leaf(1), Leaf(2)).foldedDepth shouldBe 1
    Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)).foldedDepth shouldBe 2
  }

  it should "calculate foldedMap correctly" in {
    Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)).foldedMap(_.toString) shouldBe Branch(Branch(Leaf("1"), Leaf("2")), Leaf("3"))
    Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)).foldedMap(_*2) shouldBe Branch(Branch(Leaf(2), Leaf(4)), Leaf(6))
  }

  "A Tree[Int]" should "find the max value" in {
    Tree.maximum(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))) shouldBe 3
    Tree.maximum(Branch(Branch(Leaf(-1), Leaf(-2)), Leaf(-3))) shouldBe -1
  }

  it should "find the foldedMax value" in {
    Tree.foldedMaximum(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))) shouldBe 3
    Tree.foldedMaximum(Branch(Branch(Leaf(-1), Leaf(-2)), Leaf(-3))) shouldBe -1
  }
}
