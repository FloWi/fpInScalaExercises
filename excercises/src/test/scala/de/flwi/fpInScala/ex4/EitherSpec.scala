package de.flwi.fpInScala.ex4

import org.scalatest.{FlatSpec, Matchers}

import scala.{Either => _, Option => _, Some => _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

class EitherSpec extends FlatSpec with Matchers {

  "Either" should "map correctly" in {
    Some(1).map(_+1) shouldBe Some(2)
    (None: Option[Int]).map(_+1) shouldBe None
  }
}
