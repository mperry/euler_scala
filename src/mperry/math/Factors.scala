package mperry.math

object Factors {

  
  
  def factors(n: BigInt): List[BigInt] = {
    factors(n, 1, Math.sqrt(n.toDouble).toInt, List()).sort(_ > _).tail.reverse
  }
  
  def factors(numerator: BigInt, denominator: BigInt, to: BigInt, results: List[BigInt]): List[BigInt] = {
    if (denominator > Math.sqrt(numerator.toDouble).toInt) results
    else if (numerator % denominator == 0) factors(numerator, denominator + 1, to, denominator :: numerator / denominator :: results) 
    else factors(numerator, denominator + 1, to, results)
  }

   def factors2(numerator: BigInt, s: Stream[Int], index: Int, results: List[BigInt]): List[BigInt] = {
     val denominator = s(index)
    if (denominator == numerator) denominator::results 
    else if (numerator % denominator == 0) 
    factors2(numerator / denominator, s, index + 1, denominator :: (numerator / denominator):: results) 
    else factors2(numerator, s, index + 1, results)
  }

  
}