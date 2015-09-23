package de.flwi.fpInScala.ex4

import org.scalatest.{FlatSpec, Matchers}

import scala.{Either => _, Option => _, Some => _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter
import scala.util.{Try => _}
import de.flwi.fpInScala.ex4.Either._

class EitherSpec extends FlatSpec with Matchers {

  val failed = Left("exception"): Either[String, Int]
  val successful = Right(1): Either[String, Int]

  "Either" should "map correctly" in {
    failed.map(_+2) shouldBe failed
    successful.map(_+2) shouldBe Right(3)
  }

  it should "flatMap correctly" in {
    failed.flatMap(a => Try(a+1)) shouldBe failed
    successful.flatMap(a => Try(a+1)) shouldBe Right(2)
  }

  it should "orElse correctly" ignore {
    Some(1).orElse(Some(2)) shouldBe Some(1)
    (None: Option[Int]).orElse(Some(2)) shouldBe Some(2)
  }


}
