package de.flwi.fpInScala.ex6

import de.flwi.fpInScala.ex6.RNG.SimpleRNG
import org.scalatest.{FlatSpec, Matchers}

import RNG._

class StateSpec  extends FlatSpec with Matchers {

  "RNG" should "generate nonNegativeInt correclty" in {
    1.to(100*1000)
      .map(seed => nonNegativeInt(SimpleRNG(seed))._1)
      .forall(_ >= 0) shouldBe true
  }

  it should "generate a double [0,1) correctly" in {
    1.to(100*1000)
      .map(seed => double(SimpleRNG(seed))._1)
      .forall(r => r >= 0.0 && r < 1.0) shouldBe true
  }


}
