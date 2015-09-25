package de.flwi.fpInScala.ex6

import scala.annotation.tailrec


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = SimpleRNG(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

// /*old impl*/
//  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
//    rng => {
//      val (a, rng2) = s(rng)
//      (f(a), rng2)
//    }

  def map[A,B](s: Rand[A])(f:A=>B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val res@(value, nextRNG) = int(rng)

    if(value < 0) {
      val newValue = if(value == Int.MinValue) Int.MaxValue else -value
      (newValue, nextRNG)
    } else res
  }

  /*
This will certainly generate a number in the range, but it’ll be skewed because Int.MaxValue may not be exactly divisible by n. So numbers that are less than the remainder of that division will come up more frequently. When nonNegativeInt generates numbers higher than the largest multiple of n that fits in a 32-bit integer, we should retry the generator and hope to get a smaller number.
   */
  def skewedNonNegativeLessThan(n: Int): Rand[Int] =
    map(nonNegativeInt) { _ % n }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if(i + (n-1)-mod >= 0) unit(mod) else nonNegativeLessThan(n) //<-- map is not enough here - flatMap to the rescue
    }

  def nonNegativeLessThanWithoutFlatMapFromBook(n: Int): Rand[Int] = { rng =>
    val (i, rng2) = nonNegativeInt(rng)
    val mod = i % n
    if (i + (n-1) - mod >= 0)
      (mod, rng2)
    else nonNegativeLessThan(n)(rng)
  }

  def double: Rand[Double] = {
    map(nonNegativeInt)(value => value.toDouble / Int.MaxValue)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, rng1) = int(rng)
    val (d, rng2) = double(rng)
    ((i,d), rng2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (d, rng1) = double(rng)
    val (i, rng2) = int(rng1)
    ((d,i), rng2)
  }

  def randIntDouble: Rand[(Int, Double)] = both(int, double)

  def randDoubleInt: Rand[(Double, Int)] = both(double, int)


  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2)

    ((d1,d2,d3), rng3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {

    @tailrec
    def helper(remainder: Int, currentRNG: RNG, result: List[Int]): (List[Int], RNG) = {
      if(remainder == 0) (result.reverse, currentRNG)
      else {
        val (i, nextRNG) = int(currentRNG)

        helper(remainder - 1, nextRNG, i :: result)
      }
    }

    helper(count, rng, Nil)
  }

  def intsViaSequence(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

//  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
//    rng =>
//      val (a, rng1) = ra(rng)
//      val (b, rng2) = rb(rng1)
//      (f(a, b), rng2)
//  }
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(ra)(a => map(rb)(b => f(a, b)))

// for-comprehension doesn't work yet, because flatMap and map aren't instance-methods
//    for {
//      a <- ra
//      b <- rb
//    } yield f(a,b)
  }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((_, _))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = rng => {

    @tailrec
    def helper(curRNG: RNG, remainingFs: List[Rand[A]], result: List[A]): (List[A], RNG) = {
      remainingFs match {
        case Nil => (result.reverse, curRNG)
        case h :: tail =>
          val (a, nextRNG) = h(curRNG)
          helper(nextRNG, tail, a :: result)
      }
    }
    helper(rng, fs, Nil)
  }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, rng2) = f(rng)
    g(a)(rng2)
  }
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    sys.error("todo")
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    sys.error("todo")
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    sys.error("todo")
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
