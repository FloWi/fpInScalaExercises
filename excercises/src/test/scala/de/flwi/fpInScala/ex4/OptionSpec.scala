package de.flwi.fpInScala.ex4

import org.scalatest.{Matchers, FlatSpec}
import scala.{Either => _, Option => _, Some => _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

class OptionSpec extends FlatSpec with Matchers {

  "An Option" should "map correctly" in {
    Some(1).map(_+1) shouldBe Some(2)
    (None: Option[Int]).map(_+1) shouldBe None
  }

  it should "getOrElse correctly" in {
    Some(1).getOrElse(2) shouldBe 1
    (None: Option[Int]).getOrElse(2) shouldBe 2
  }

  it should "flatMap correctly" in {
    Some(1).flatMap(i => Some(i+1)) shouldBe Some(2)
    (None: Option[Int]).flatMap(i => Some(i+1)) shouldBe None
  }

  it should "orElse correctly" in {
    Some(1).orElse(Some(2)) shouldBe Some(1)
    (None: Option[Int]).orElse(Some(2)) shouldBe Some(2)
  }

  it should "filter correctly" in {
    Some(1).filter(_ % 2 == 0) shouldBe None
    Some(2).filter(_ % 2 == 0) shouldBe Some(2)
    (None: Option[Int]).filter(_ % 2 == 0) shouldBe None
  }

  "Option.mean" should "be calculated correctly" in {
    Option.mean(Seq(2,4)) shouldBe Some(3.0)
    Option.mean(Seq.empty) shouldBe None
  }

  "Option.map2" should "lift f correctly" in {
    Option.map2(Some(1), Some(2))(_+_) shouldBe Some(3)
    Option.map2(None: Option[Int], Some(2))(_+_) shouldBe None
    Option.map2(Some(1), None: Option[Int])(_+_) shouldBe None
  }

  "Option.sequence" should "convert a list of options properly into an Option of List" in {
    Option.sequence(List(Some(1), Some(2), Some(3))) shouldBe Some(List(1,2,3))
    Option.sequence(List(Some(1), Some(2), None)) shouldBe None
    Option.sequence(List.empty) shouldBe None
  }

  "Option.traverse" should "convert a list properly into an Option of List" in {
    Option.traverse(List(1,2,3))(x => Some(x*2)) shouldBe Some(List(2,4,6))
    Option.traverse(List(1,2,3))(x => if(x % 2 == 0) Some(x*2) else None) shouldBe None
    Option.traverse(List(1,2,3))(x => None) shouldBe None
    Option.traverse(List.empty[Int])(x => Some(x*2)) shouldBe None
  }

  "Option.traverse2" should "convert a list properly into an Option of List" in {
    Option.traverse2(List(1,2,3))(x => Some(x*2)) shouldBe Some(List(2,4,6))
    Option.traverse2(List(1,2,3))(x => if(x % 2 == 0) Some(x*2) else None) shouldBe None
    Option.traverse2(List(1,2,3))(x => None) shouldBe None
    Option.traverse2(List.empty[Int])(x => Some(x*2)) shouldBe None
  }

}
