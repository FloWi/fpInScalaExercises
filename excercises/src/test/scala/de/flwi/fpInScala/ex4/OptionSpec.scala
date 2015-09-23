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

}
