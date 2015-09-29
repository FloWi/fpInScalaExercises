package de.flwi.fpInScala.ex6

import org.scalatest.{Matchers, FlatSpec}

class CandyMachineSpec  extends FlatSpec with Matchers {

  import Candy._

  "A locked Machine with candy" should "unlock after inserting a coin" in {
    simulateMachine(List(Coin)).run(Machine(locked = true, candies = 4, coins = 10))._2 shouldBe Machine(locked = false, candies = 4, coins = 11)
  }

  "A locked Machine with no candy left should" should "not unlock after inserting a coin" in {
    simulateMachine(List(Coin)).run(Machine(locked = true, candies = 0, coins = 10))._2 shouldBe Machine(locked = true, candies = 0, coins = 10)
  }

  "An unlocked machine with candy" should "dispense a candy after turning the knob and become locked again" in {
    simulateMachine(List(Turn)).run(Machine(locked = false, candies = 4, coins = 10))._2 shouldBe Machine(locked = true, candies = 3, coins = 10)
  }

  it should "do nothing when a coin is being inserted" in {
    val machine: Machine = Machine(locked = false, candies = 4, coins = 10)
    simulateMachine(List(Coin)).run(machine)._2 shouldBe machine
  }
}

