package mperry

object P5 {

  /*
  2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without 
  any remainder.  What is the smallest positive number that is evenly divisible by all of 
  the numbers from 1 to 20?
	*/
  
  def p() = {
//    primesProduct
    val v = smallestDivisible(20)
    println(v)
  }

  def primesProduct = {
    val r = primes2(Stream.Empty, 2, 10)
    val n = r.foldLeft(1) (_ * _)
    println("r = " + r.print + " n = " + n)
  }
 
  def primes2(s: Stream[Int], n: Int, max: Int): Stream[Int] = {
	  if (n > max) s
	  else if (s.forall(x => n % x != 0)) primes2(s append Stream(n), n + 1, max)
	  else primes2(s, n + 1, max)
  }
  
  def smallestDivisible(n: Int): Int = {
	  smallestDivisible(n, n)
  }

  def smallestDivisible(num: Int, step: Int): Int = {
	  if ((1 to step).forall(n => num % n == 0)) num
	  else smallestDivisible(num + step, step)
  }

  
}