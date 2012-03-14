package mperry
import scala.Option.option2Iterable

import mperry.math.PrimeSieve

/**
 * The prime 41, can be written as the sum of six consecutive primes:
 * 41 = 2 + 3 + 5 + 7 + 11 + 13
 *
 * This is the longest sum of consecutive primes that adds to a prime below one-hundred.
 *
 * The longest sum of consecutive primes below one-thousand that adds to a prime,
 * contains 21 terms, and is equal to 953.
 *
 * Which prime, below one-million, can be written as the sum of the most consecutive
 * primes?
 *
 * Note that the solution for 953 above is:
 * (953,List(7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89))
 * 
 */
object P50 {

  def p = {
    val s = PrimeSieve.lazyValSieve
//    val p = primesBelow(s, 1000, 0, Nil).reverse
    val high = 1000
    val high2 = Math.pow(10, 6).toInt
    val p = primeSumBelow(s, high2)
    val sizes = p.map(_._2.size)
    val max = sizes.max
    val index = sizes.indexWhere(_ == max)
    
    println("size = " + max + " list = " + p(index))
  }

  def primesBelow(s: Stream[Int], high: Int, index: Int, acc: List[Int]): List[Int] = {
    val n = s.slice(0, index).fold(0)(_ + _)
    // is this too high and then a prime
    //	  val high = 10
    if (n > high) {
      acc
    } else {
      val i = s.findIndexOf(_ >= n)
      if (s(i) == n) {
        primesBelow(s, high, index + 1, n :: acc)
      } else {
        primesBelow(s, high, index + 1, acc)
      }

    }
  }

  def getforme(s: Stream[Int], i: Int, j: Int, high: Int): Option[(Int, List[Int])] = {
     val slice = s.slice(i, j)
      val sum = slice.sum
      val index = s.indexWhere(_ >= sum)
      val o = if (index < 0 || sum > high) {
        None
      } else {
          if (s(index) == sum) {
        	  Some((sum, slice.toList))
          } else {
            None
          }
        
      }
//      val o = index match {
//        case None => None
//        case Some(z) =>
//                    if (s(z) == sum) {
//        	  Some(sum)
//          } else {
//            None
//          }
//
//        }
     return o
  	}
  
  
  def primeSumBelow(s: Stream[Int], high: Int): List[(Int, List[Int])] = {
    val highIndex = s.findIndexOf(_ >= high)
    val v = for (
      i <- 0 to highIndex; j <- (i + 2).to(highIndex)
    ) yield getforme(s, i, j, high)
    v.toList.flatten
  }

}