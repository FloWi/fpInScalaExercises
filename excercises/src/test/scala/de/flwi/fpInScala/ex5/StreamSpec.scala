package de.flwi.fpInScala.ex5

import org.scalatest.{Matchers, FlatSpec}

class StreamSpec  extends FlatSpec with Matchers {

  "A stream" should "convert to a list correctly" in {
    Stream(1,2,3).toList shouldBe List(1,2,3)
    Stream().toList shouldBe Nil
  }

  it should "take(n) correctly" in {
    Stream(1.to(10): _*).take(3).toList shouldBe List(1, 2, 3)
  }

  it should "drop(n) correctly" in {
    Stream(1.to(10): _*).drop(3).toList shouldBe 4.to(10).toList
  }

  it should "takeWhile correctly" in {
    Stream(1.to(10): _*).takeWhile(_ < 5).toList shouldBe 1.to(4).toList
    Stream.empty[Int].takeWhile(_ < 5).toList shouldBe Nil
  }

  it should "takeWhileViaFoldRight correctly" in {
    Stream(1.to(10): _*).takeWhileViaFoldRight(_ < 5).toList shouldBe 1.to(4).toList
    Stream.empty[Int].takeWhileViaFoldRight(_ < 5).toList shouldBe Nil
  }

  it should "forAll correctly" in {
    Stream(2.to(10, 2): _*).forAll(_ % 2 == 0) shouldBe true
    Stream(1.to(10): _*).forAll(_ % 2 == 0) shouldBe false
  }
}
