package mperry

/*
 * By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that 
 * the 6th prime is 13.
 *
 * What is the 10 001st prime number?
 * 
 */
object P7 {
  
  def p = {
    val primes = sieveWithSize(10001, stopOnListSize, debug).reverse
    val p = primes.last
    assert(p == 104743)
    println("p = " + p + " size = " + primes.size + " list = " + primes)
  }

  def stopOnListSize(a: List[Int], current: Int, max: Int): Boolean = {
    a.size >= max
  }
  
  def stopWithLmit(a: List[Int], current: Int, max: Int): Boolean = {
    current >= max
  }
  
  def debug(current: Int, max: Int) {
    
  }
  
  def sieveWithSize(max: Int, f: (List[Int], Int, Int) => Boolean, debug: (Int, Int) => Unit): List[Int] = {
	  sieveWithSize(2::Nil, 3, max, f, debug)
  }
  
  def sieveWithSize(a: List[Int], current: Int, max: Int, 
      f: (List[Int], Int, Int) => Boolean, debug: (Int, Int) => Unit): List[Int] = {
    debug(current, max)
    val step = 2
    if (f(a, current, max)) a
    else if (a.forall(x => current % x != 0)) sieveWithSize(current::a, current + step, max, f, debug)
    else sieveWithSize(a, current + step, max, f, debug)
  }
  
}
