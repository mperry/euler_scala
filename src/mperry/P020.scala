package mperry
/**
 * 
 * n! means n (n 1) ... 3 2 1
 * 
 * For example, 10! = 10 9 ... 3 2 1 = 3628800,
 * and the sum of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.
 * 
 * Find the sum of the digits in the number 100!
 *  
 */
object P020 {

  def p = {
   def s = factorialDigits(100)
   def f = fact(100)
   println("digit sum = " + s + " fac = " + f)
   assert(s == 648)
  }
  
  def factorialDigits(n: Int): BigInt = {
    fact(n).toString.map(_.getNumericValue).fold(0)(_+_)
  }
  
  def fact(n: Int): BigInt = {
    n match {
      case 1 => 1
      case _ => n * fact(n - 1)
    }
  }
  
}
