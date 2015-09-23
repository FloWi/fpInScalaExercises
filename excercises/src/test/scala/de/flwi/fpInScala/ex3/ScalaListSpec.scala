package de.flwi.fpInScala.ex3

import org.scalatest.{FlatSpec, Matchers}

class ScalaListSpec extends FlatSpec with Matchers {

  "scala list" should "show the difference between reduceLeft, foldLeft and scanLeft" in {
    val abc = List("A", "B", "C")

    def add(res: String, x: String) = {
      println(s"op: $res + $x = ${res + x}")
      res + x
    }

    println("\n------------------")
    println("reduceLeft")
    abc.reduceLeft(add)
    // op: A + B = AB
    // op: AB + C = ABC    // accumulates value AB in *first* operator arg `res`
    // res: String = ABC


    println("\n------------------")
    println("foldLeft")
    abc.foldLeft("z")(add) // with start value "z"
    // op: z + A = zA      // initial extra operation
    // op: zA + B = zAB
    // op: zAB + C = zABC
    // res: String = zABC


    println("\n------------------")
    println("scanLeft")
    abc.scanLeft("z")(add)
    // op: z + A = zA      // same operations as foldLeft above...
    // op: zA + B = zAB
    // op: zAB + C = zABC
    // res: List[String] = List(z, zA, zAB, zABC) // maps intermediate results
  }


  it should "show the difference between reduceRight, foldRight and scanRight" in {
    val abc = List("A", "B", "C")

    def add(x: String, res: String) = {
      println(s"op: $x + $res = ${x + res}")
      x + res
    }

    println("\n------------------")
    println("reduceRight")
    abc.reduceRight(add)
    // op: B + C = BC
    // op: A + BC = ABC  // accumulates value BC in *second* operator arg `res`
    // res: String = ABC


    println("\n------------------")
    println("foldRight")
    abc.foldRight("z")(add)
    // op: C + z = Cz
    // op: B + Cz = BCz
    // op: A + BCz = ABCz
    // res: String = ABCz

    println("\n------------------")
    println("scanRight")
    abc.scanRight("z")(add)
    // op: C + z = Cz
    // op: B + Cz = BCz
    // op: A + BCz = ABCz
    // res: List[String] = List(ABCz, BCz, Cz, z)

  }

}
