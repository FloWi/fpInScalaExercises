package de.flwi.fpInScala.ex6

import de.flwi.fpInScala.ex6.RNG.SimpleRNG
import org.scalatest.{FlatSpec, Matchers}

import RNG._

class StateSpec  extends FlatSpec with Matchers {

  "RNG" should "generate nonNegativeInt correclty" in {
    nonNegativeInt(SimpleRNG(1L))._1 should be >= 0

    1.to(100*1000).map(i => nonNegativeInt(SimpleRNG(i))._1).forall(r => r >= 0) shouldBe true
  }

}
