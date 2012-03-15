package mperry
import mperry.math.Factors
/**
 * 
 * Let d(n) be defined as the sum of proper divisors of n (numbers less than n which 
 * divide evenly into n).
 * 
 * If d(a) = b and d(b) = a, where a b, then a and b are an amicable pair and each of 
 * a and b are called amicable numbers.
 * 
 * For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; 
 * therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71 and 142; 
 * so d(284) = 220.
 * 
 * Evaluate the sum of all the amicable numbers under 10000.
 * 
 */
object P021 {

  def p = {
    def pairs = (1 until 10000).toList.flatMap(pair(_))
    def duplicatedList = myFold(List(), pairs)
    def sorted = duplicatedList.toSet.toList.sort(_ <= _)
    def total = sorted.fold(0) (_ + _ )
    println("pairs = " + pairs)
    println("list = " + sorted)
    println("total = " + total)
    assert(total == 40285)
  }
  
  def d(n: Int): Int = {
    val f = Factors.factors(n)
    f.sum.toInt
  }
  
  def myFold(result: List[Int], it: List[(Int, Int)]): List[Int] = {
    it match {
      case Nil => result
      case (h::t) => myFold(h._1:: h._2 :: result, t)
    }
  }
  
  def pair(a: Int): Option[(Int, Int)] = {
    def b = d(a)
    if ((d(b)) == a) {
      return Some((a, b))
    }
    None
  }
  
  def amicable(a: Int, b: Int): Boolean = {
    d(a) == b && d(b) == a
  }
  
  def isAmicale(a: Int): Boolean = {
    d(d(a)) == a
  }
  
}
