package mperry

/**
 * If a box contains twenty-one coloured discs, composed of fifteen blue discs and
 * six red discs, and two discs were taken at random, it can be seen that the
 * probability of taking two blue discs, P(BB) = (15/21)x(14/20) = 1/2.
 *
 * The next such arrangement, for which there is exactly 50% chance of taking two
 * blue discs at random, is a box containing eighty-five blue discs and thirty-five
 * red discs.
 *
 * By finding the first arrangement to contain over 10^12 = 1,000,000,000,000 discs
 * in total, determine the number of blue discs that the box would contain.
 *
 */
object P100 {

  def p = {
    val total = 21
    val blue = 15;
    val red = total - blue;

    //    val o = odds(2, blue, total)
    //    val b = o == BigDecimal(0.5)

//    val max = 100
        val max = BigInt(10).pow(2)
//    val v = findMax(2, max)
//    println(v)
    
    val s = findStream(2)
    val i = s.indexWhere(process(_, max.<))
    val z  = s.flatten
    println ("i = " + i + " v = " + s(i) + " s = " + s)
    println(z.take(3).print)
    
    
  }

  def process(o: Option[(BigInt, BigInt)], f: BigInt => Boolean): Boolean = {
      o match {
        case None => false
        case Some(x) => f(x._2)
      }
      
    
  }

  def findStream(choose: Int): Stream[Option[(BigInt, BigInt)]] = {
    val s = Stream.from(choose)
    s.map(findMax(choose, _))
  }

  
  def findAllMax(choose: BigInt, max: BigInt) = {
    
  }
  
  /*
   * test blue discs out of total
   */
  def findMax(choose: BigInt, max: BigInt): Option[(BigInt, BigInt)] = {
    val l = for (ok <- choose to max; if (odds(choose, ok, max) == 0.5))
      yield (ok, max)
      if (l.size == 0) {
        None
      } else {
        Some(l.head)
      }
  }

  def odds(num: BigInt, good: BigInt, total: BigInt): BigDecimal = {
    if (num <= 0) {
      1
    } else {
      val a = BigDecimal(good) / BigDecimal(total)
      return a * odds(num - 1, good - 1, total - 1)
    }

  }

}

