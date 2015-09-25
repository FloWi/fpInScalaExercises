package de.flwi.fpInScala.ex6

import de.flwi.fpInScala.ex6.RNG.SimpleRNG
import org.scalatest.{FlatSpec, Matchers}

import RNG._

import scala.collection.immutable

class StateSpec  extends FlatSpec with Matchers {

  private val n: Int = 5000

  "RNG" should "generate nonNegativeInt correclty" in {
    1.to(n)
      .map(seed => nonNegativeInt(SimpleRNG(seed))._1)
      .forall(_ >= 0) shouldBe true
  }

  it should "generate a double [0,1) correctly" in {
    1.to(n)
      .map(seed => double(SimpleRNG(seed))._1)
      .forall(r => r >= 0.0 && r < 1.0) shouldBe true
  }

  it should "generate intDouble correctly" in {
    val (ints, doubles) = 1.to(n)
      .map(seed => intDouble(SimpleRNG(seed))._1)
      .unzip

    ints.distinct.size shouldBe n
    doubles.distinct.size shouldBe n
  }

  it should "generate doubleInt correctly" in {
    val (doubles, ints) = 1.to(n)
      .map(seed => doubleInt(SimpleRNG(seed))._1)
      .unzip

    ints.distinct.size shouldBe n
    doubles.distinct.size shouldBe n
  }

  it should "generate double3 correctly" in {
    val doubles = 1.to(n)
      .map(seed => double3(SimpleRNG(seed))._1)

    doubles.map(_._1).distinct.size shouldBe n
    doubles.map(_._2).distinct.size shouldBe n
    doubles.map(_._3).distinct.size shouldBe n
  }
}
