package de.flwi.fpInScala.ex5

import org.scalatest.{Matchers, FlatSpec}

class StreamSpec  extends FlatSpec with Matchers {

  "A stream" should "convert to a list correctly" in {
    Stream(1,2,3).toList shouldBe List(1,2,3)
    Stream().toList shouldBe Nil
  }
}
