package mperry
import mperry.math.Factors

/*
 * The prime factors of 13195 are 5, 7, 13 and 29.
 * 
 * What is the largest prime factor of the number 600851475143 ?
 * 
 */
object P03 {

  def p = {
	  val f = Factors.factors(BigInt("600851475143"))
	  val high = f.last
	  assert(high == 6857)
	  println ("highest_factor = " + high + " factors = " + f)
  }
  
  
}
