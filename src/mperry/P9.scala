package mperry

object P9 {

  /*
   * A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
   * a2 + b2 = c2
   * 
   * For example, 32 + 42 = 9 + 16 = 25 = 52.
   * 
   * There exists exactly one Pythagorean triplet for which a + b + c = 1000.
   * Find the product abc.
   */
  def p = {
		  
    val n = 1000
    val v = for (i <- 1 to n; j <- i + 1 to n; k <- j + 1 to n if (i < j && j < k && i + j + k == n && i * i + j * j == k * k)) yield (List(i, j, k))
    println(v)
      
    
  }

  
  
  
}

