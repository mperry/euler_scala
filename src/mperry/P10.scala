package mperry

/*
 * The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
 * Find the sum of all the primes below two million.
 * 
 */

object P10 {

  def p = {
    val max = 2000000
    // executes in around 30 mins?
    val r = P7.sieveWithSize(max, P7.stopWithLmit, debug).reverse
    val x = r.foldLeft (0) (_ + _)
    assert(x == 1179908154)
    println ("sum = " + x + " list = " + r)
  }
  
  def debug(current: Int, max: Int) = {
    val n = 1000
    if ((current - 1) % n == 0) {
      println ("current = " + current + " max = " + max)
    }
  }
  
}
