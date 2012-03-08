package mperry

/*
 * The prime factors of 13195 are 5, 7, 13 and 29.
 * 
 * What is the largest prime factor of the number 600851475143 ?
 * 
 */
object P3 {

  def p = {
	  val f = factors(BigInt("600851475143"))
	  val high = f.last
	  assert(high == 6857)
	  println ("highest_factor = " + high + " factors = " + f)
  }
  
  def factors(n: BigInt): List[BigInt] = {
    factors(n, 2, List()).reverse
  }
  
  def factors(numerator: BigInt, denominator: BigInt, results: List[BigInt]): List[BigInt] = {
    if (denominator == numerator) denominator::results 
    else if (numerator % denominator == 0) factors(numerator / denominator, denominator, denominator :: results) 
    else factors(numerator, denominator + 1, results)
  }
  
}
