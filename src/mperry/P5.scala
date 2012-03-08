package mperry

/*
 * 2520 is the smallest number that can be divided by each of the numbers from 
 * 1 to 10 without any remainder.
 * 
 * What is the smallest positive number that is evenly divisible by all of the 
 * numbers from 1 to 20?
 * 
 */
object P5 {
  
  def p() = {
    val v = smallestDivisible(20)
    assert(v == 232792560)
    println(v)
  }
  
  def smallestDivisible(n: Int): Int = {
	  smallestDivisible(n, n)
  }

  def smallestDivisible(num: Int, max: Int): Int = {
	  if ((1 to max).forall(d => num % d == 0)) num
	  else smallestDivisible(num + max, max)
  }

  
}