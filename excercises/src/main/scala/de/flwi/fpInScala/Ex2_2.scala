package de.flwi.fpInScala

import scala.annotation.tailrec

object Ex2_2 extends App {

  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {

    @tailrec
    def helper(last: A, index: Int): Boolean = {

      if(index >= as.length) true
      else {
        val current = as(index)
        if(ordered(last, current)) helper(current, index+1)
        else false
      }
    }

    as.headOption match {
      case None => true
      case Some(head) => helper(head, index = 1)
    }

  }


  

}
