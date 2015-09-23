package de.flwi.fpInScala.ex4

import org.scalatest.{FlatSpec, Matchers}

import scala.{Either => _, Option => _, Some => _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

class EitherSpec extends FlatSpec with Matchers {

  "Either" should "map correctly" in {
    Either.Try(2/0).map(_+2) shouldBe Left(_: Exception)
    Either.Try(2/2).map(_+2) shouldBe Right(3)
  }

  it should "flatMap correctly" ignore {
    Some(1).flatMap(i => Some(i+1)) shouldBe Some(2)
    (None: Option[Int]).flatMap(i => Some(i+1)) shouldBe None
  }

  it should "orElse correctly" ignore {
    Some(1).orElse(Some(2)) shouldBe Some(1)
    (None: Option[Int]).orElse(Some(2)) shouldBe Some(2)
  }


}
