package mperry

/*
 * If we list all the natural numbers below 10 that are multiples of 3 or 5, 
 * we get 3, 5, 6 and 9. The sum of these multiples is 23.
 * 
 * Find the sum of all the multiples of 3 or 5 below 1000.
 * 
 */
object P1 {

	def p = {
	  val v = (1 until 1000).filter(n => n % 3 == 0 || n % 5 == 0).foldLeft (0) (_ + _)
	  assert(v == 233168)
	  println(v);
	}
	
}
