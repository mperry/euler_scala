package mperry
import mperry.math.Factors._
import mperry.math.PerfectNumber

/**
 * A perfect number is a number for which the sum of its proper divisors is exactly 
 * equal to the number. For example, the sum of the proper divisors of 28 would 
 * be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.
 * 
 * A number n is called deficient if the sum of its proper divisors is less than n and 
 * it is called abundant if this sum exceeds n.
 * 
 * As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest number 
 * that can be written as the sum of two abundant numbers is 24. By mathematical 
 * analysis, it can be shown that all integers greater than 28123 can be written 
 * as the sum of two abundant numbers. However, this upper limit cannot be reduced 
 * any further by analysis even though it is known that the greatest number that 
 * cannot be expressed as the sum of two abundant numbers is less than this limit.
 * 
 * Find the sum of all the positive integers which cannot be written as the sum of 
 * two abundant numbers.
 * 
 */
object P023 {

  val MAX = 28123
  
  def p = {
    val bigList = (1 to MAX).toList
    val nonTwo = bigList.filter(!sumTwo(_, allAbundant))
    val sum = nonTwo.sum
    println("sum = " + sum + " length = " + nonTwo.length)
    println(nonTwo)
    assert(sum == 4179871)
    assert(nonTwo.length == 1456)
  }
  
  def sumTwo(n: Int, abundant: Stream[Int]): Boolean = {
    var found = false
    val x = 1000
    if (n % x == 0) {
    	println(n)  
    }
    var i = 0
    while (!found && abundant(i) < n) {
      val a = abundant(i)
      val j = abundant.indexWhere(_ >= n - a)
      found = abundant(j) == n - a
      i += 1
    }
    found
  }
  
  val allAbundant: Stream[Int] = {
    Stream.from(1).filter(PerfectNumber.isPerfect(_) == PerfectNumber.ABUNDANT)
  }
  
}
