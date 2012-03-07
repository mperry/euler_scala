package mperry

object P10 {
  /*
   * The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
   * Find the sum of all the primes below two million.
   * 
   */

  def p = {
    
    def v = Math.pow(3, 2).toInt
//    println (v)
    
    val max = 2000000
    val m  = 10
    val r = P7.sieveWithMax(2::Nil, 2, max)
    val x = r.foldLeft (0) (_ + _)
    println (r + " " + x)
    
    
  }
  
}