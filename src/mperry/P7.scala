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
    val size = 10001
//    val primes = sieveWithSize(size, stopOnListSize, debug).reverse
//    val p = primes.last
//    val primes = lazySieve(Stream from 2).take(size)
    val primes = lazyValSieve.take(size)
    val p = primes.last
    
//    assert(p == 104743)
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
 
  /**
   * This sieve is simple and is called recursively, filtering each element
   * On my box with JVM parameter for max heap size of 1024 (-Xmx1024m) I 
   * get OutOfMemory errors because it constructs a stream in memory for each prime 
   * number 
   */
  def lazySieve(s: Stream[Int]): Stream[Int] = {
    val prime = s.head
    println(prime + " ")
    Stream.cons(prime, lazySieve(s.filter(x => x % prime != 0)))
  }
  
  /**
   * This is fast, it reuses the one stream, only checking up to root(n)
   */
  val lazyValSieve: Stream[Int] = 2 #:: Stream.from(3).filter(i => lazyValSieve.takeWhile(j => j * j <= i).forall(i % _ > 0))
  
  
}
