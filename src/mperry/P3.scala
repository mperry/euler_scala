package mperry

object P3 {

  def p = {
	  val r = factor(BigInt("600851475143"))
	  val r2 = r.head
	  println ("r = " + r + " r2 = " + r2)
  }
  
  def factor(n: BigInt): List[BigInt] = {
    factor(n, 2, List())
  }
  
  def factor(n: BigInt, d: BigInt, a: List[BigInt]): List[BigInt] = {
    if (d == n) d:: a 
    else if (n % d == 0) factor(n / d, d, d :: a) 
    else factor(n, d + 1, a)
  }
  
}