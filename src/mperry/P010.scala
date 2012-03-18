package mperry

/*
 * The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
 * Find the sum of all the primes below two million.
 * 
 */

object P010 {

  def p = {
    val max = 2000000
    // executes in around 30 mins?
    val r = P007.sieveWithSize(max, P007.stopWithLmit, debug).reverse
    val x = r.foldLeft (0) (_ + _)
    println ("sum = " + x + " list = " + r)
    assert(x == 1179908154)
  }
  
  def debug(current: Int, max: Int) = {
    val n = 1000
    if ((current - 1) % n == 0) {
      println ("current = " + current + " max = " + max)
    }
  }
  
}
