package de.flwi.fpInScala.ex4

import org.scalatest.{FlatSpec, Matchers}

import scala.{Either => _, Option => _, Some => _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter
import scala.util.{Try => _}
import de.flwi.fpInScala.ex4.Either._

class EitherSpec extends FlatSpec with Matchers {

  val failed = Left("exception"): Either[String, Int]
  def successful(n: Int): Either[String, Int] = Right(n)

  "Either" should "map correctly" in {
    failed.map(_+2) shouldBe failed
    successful(1).map(_+2) shouldBe Right(3)
  }

  it should "flatMap correctly" in {
    failed.flatMap(a => Try(a+1)) shouldBe failed
    successful(1).flatMap(a => Try(a+1)) shouldBe Right(2)
  }

  it should "orElse correctly" in {
    failed.orElse(successful(1)) shouldBe successful(1)
    successful(1).orElse(failed) shouldBe successful(1)
    successful(2).orElse(successful(1)) shouldBe successful(2)
    failed.orElse(failed) shouldBe failed
  }

  it should "lift f correctly with map2" in {
    successful(1).map2(failed)(_+_) shouldBe failed
    failed.map2(successful(2))(_+_) shouldBe failed
    failed.map2(failed)(_+_) shouldBe failed

    successful(1).map2(successful(2))(_+_) shouldBe successful(3)
  }

  it should "sequence correctly" in {
    // Implement sequence and traverse for Either. These should return the first error that’s encountered, if there is one.
    Either.sequence(List(successful(1), successful(2), successful(3))) shouldBe (Right(List(1,2,3)): Either[String, List[Int]])
    Either.sequence(List(failed, successful(2), successful(3))) shouldBe (Left("exception"): Either[String, List[Int]])
  }

  it should "traverse correctly" in {
    // Implement sequence and traverse for Either. These should return the first error that’s encountered, if there is one.
    Either.traverse(List(0,1,2,3))(a => Try(a*2)) shouldBe (Right(List(0, 2,4,6)): Either[String, List[Int]])
    Either.traverse(List(0,1,2,3))(a =>
      try {Right(a / 0)}
      catch { case ex: Exception => Left("failed")}
    ) shouldBe (Left("failed"): Either[String, List[Int]])
  }
}
