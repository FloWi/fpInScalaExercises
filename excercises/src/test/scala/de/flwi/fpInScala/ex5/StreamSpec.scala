package de.flwi.fpInScala.ex5

import de.flwi.fpInScala.ex2.Ex2_1
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.mutable.ArrayBuffer

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

  it should "return headOption correctly and only evaluate the first element" in {
    val buf = ArrayBuffer.empty[Int]
    import Stream.cons
    def f(n: Int): Int = {
      buf += n
      n
    }
    //empty is being used by matchers
    val stream_1_2: Stream[Int] = cons(f(1), cons(f(2), Stream.empty[Int]))
    stream_1_2.headOption shouldBe Some(1)
    buf.toList shouldBe List(1)
  }

  it should "execute the elements in Stream.apply (as stated in the chapter notes)" in {
    //https://github.com/fpinscala/fpinscala/wiki/Chapter-5:-Strictness-and-laziness#streamapply

    val buf = ArrayBuffer.empty[Int]
    def f(n: Int): Int = {
      buf += n
      n
    }
    Stream(f(1), f(2))

    buf.size shouldBe 2
  }

  it should "return None as headOption of an empty Stream" in {
    Stream().headOption shouldBe None
  }

  it should "map correctly" in {
    Stream(1,2,3).map(_.toString).toList shouldBe List("1", "2", "3")
  }

  it should "filter correctly" in {
    Stream(1.to(10): _*).filter(_ % 2 == 0).toList shouldBe List(2,4,6,8,10)
    Stream.empty[Int].filter(_ % 2 == 0).toList shouldBe Nil
  }

  it should "append correctly" in {
    Stream(1,2).append(Stream(3,4)).toList shouldBe List(1,2,3,4)
    Stream(1,2).append(Stream()).toList shouldBe List(1,2)
    Stream().append(Stream(3,4)).toList shouldBe List(3,4)
  }

  it should "flatMap correctly" in {
    Stream(1,2).flatMap(Stream(_)).toList shouldBe List(1,2)
  }

  "Stream.constant" should "generate an infinite number of values" in {
    val list: List[Int] = Stream.constant(1).take(10000).toList
    list.size shouldBe 10000
    list.distinct.head shouldBe 1
  }

  "Stream.from" should "generate an ascending Stream of integers" in {
    Stream.from(1).take(2).toList shouldBe List(1,2)
    Stream.from(Int.MaxValue).take(2).toList shouldBe List(Int.MaxValue,Int.MinValue)
  }

  "Stream.fibs" should "generate a Stream of fibonacci numbers" in {
    Stream.fibs.take(10).toList shouldBe 0.to(9).map(Ex2_1.fibonacciTailRec).toList
  }

  "Stream.unfold" should "work correctly with an infinite stream" in {
    Stream.unfold(1)(s => Some(s, s+1)).take(10).toList shouldBe 1.to(10).toList
  }

  it should "work correctly with an ending stream" in {
    Stream.unfold(1)(s => if(s < 5) Some(s, s+1) else None).toList shouldBe 1.to(4).toList
  }

  "Stream.fibsViaUnfold" should "generate a Stream of fibonacci numbers" in {
    Stream.fibsViaUnfold.take(10).toList shouldBe 0.to(9).map(Ex2_1.fibonacciTailRec).toList

  }
}
