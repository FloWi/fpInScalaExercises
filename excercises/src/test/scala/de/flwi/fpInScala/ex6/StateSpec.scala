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

  it should "generate randIntDouble correctly" in {
    val (ints, doubles) = 1.to(n)
      .map(seed => randIntDouble(SimpleRNG(seed))._1)
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

  it should "generate randDoubleInt correctly" in {
    val (doubles, ints) = 1.to(n)
      .map(seed => randDoubleInt(SimpleRNG(seed))._1)
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

  it should "generate ints correctly" in {
    val (integers, _) = ints(n)(SimpleRNG(1))

    integers.distinct.size shouldBe n
  }

  it should "map2 correctly" in {
    //same seed, same result
    val (res1, _) = map2(int, int)((a,b) => a+b)(SimpleRNG(1))
    val (res2, _) = map2(int, int)((a,b) => a+b)(SimpleRNG(1))
    res1 shouldBe res2
  }

  it should "sequence(int, int) should behave exactly like both(int, int)" in {
    val (l1 :: l2 :: Nil, lRNG) = sequence(List(int, int))(SimpleRNG(1))
    val ((t1, t2), tRNG) = both(int, int)(SimpleRNG(1))

    l1 shouldBe t1
    l2 shouldBe t2
    lRNG shouldBe tRNG
  }

  it should "compute ints and intsViaSequence identical" in {
    ints(n)(SimpleRNG(1)) shouldBe intsViaSequence(n)(SimpleRNG(1))
  }

  it should "calculate nonNegativeLessThan" in {
    val rng: SimpleRNG = SimpleRNG(1)
    1.to(n)
      .map(i => (nonNegativeLessThan(i)(rng), nonNegativeLessThanWithoutFlatMapFromBook(i)(rng)))
      .forall(tup => tup._1 == tup._2) shouldBe true
  }

}
