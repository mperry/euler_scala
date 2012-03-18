package mperry

/*
 * A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
 * a2 + b2 = c2
 * 
 * For example, 3^2 + 4^2 = 9 + 16 = 25 = 52.
 * 
 * There exists exactly one Pythagorean triplet for which a + b + c = 1000.
 * Find the product abc.
 */
object P009 {

  def p = {
    val n = 1000
    val v = for (
      i <- 1 to n; j <- i + 1 to n; k <- j + 1 to n if (i < j && j < k && i + j + k == n && i * i + j * j == k * k)
    ) yield (List(i, j, k))

    assert(v.size == 1) 
    val list = v(0)
    def product = list.fold(1) (_ * _)
    println("list = " + list + " product = " + product)
    assert(list(0) == 200 && list(1) == 375 && list(2) == 425 && product == 31875000)
  }

}

