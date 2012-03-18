package mperry
import mperry.math.PrimeSieve

/**
 *
 * The primes 3, 7, 109, and 673, are quite remarkable. By taking any two
 * primes and concatenating them in any order the result will always be
 * prime. For example, taking 7 and 109, both 7109 and 1097 are prime.
 * The sum of these four primes, 792, represents the lowest sum for a set
 * of four primes with this property.
 *
 * Find the lowest sum for a set of five primes for which any two primes
 * concatenate to produce another prime.
 *
 */
object P060 {

  // TODO
  
  val primes = PrimeSieve.lazyValSieve
  
  def p = {
    findSet(2)
  }
  
  def p2 = {

    def x = test4(primes, 792)
    def y = x.map(_.map(primes(_)));
    println("x = " + x)
    println("y = " + y)
  }  
  
  def indexToPrime(list: List[List[Int]]) = {
    list.map(_.map(primes(_)));
  }
  
  def findSet(size: Int) = {
    
      
      val sol = Stream.from(792).map(test4(primes, _)).filter(_.size > 0)
      val p = sol.map(indexToPrime(_))
      val special = p.filter(x => x.filter(y => concatProperty(y)).size > 0)
      val x = special.take(1)
      
      println(x)
      
//      val v = test2(s, 36)
//      println(v)
//      generateLowStream(size)
//      Stream.from(2)
      // filter for prime sum
//      filter(Stream.from(2))
      // filter for concatenation property
    
//      lowCombos(0, s, s, s)
      
    
  }
  
  def concatProperty(nums: List[Int]): Boolean = {
    
    // get first answer
    val oracle = (nums(0).toString + nums(1).toString()).toInt
    
    // yield those that do not hold
    val v = for (i <- 0 until nums.size; j <- i + 1 until nums.size; 
    if(!isPrime((i.toString.+(j.toString)).toInt))
      ) 
      yield ((i, j))
    
    return v.isEmpty
    
  }
  
  def isPrime(i: Int): Boolean = {
    def s = PrimeSieve.lazyValSieve
    s(s.indexWhere(_ >= i)) == i
    
  }
  
  def test2(primes: Stream[Int], n: Int): List[List[Int]] = {
    val p = primes.takeWhile(_ <= n)
	val s = for (i <- 0 until p.size; j <- i + 1 until p.size; if (primes(i) + primes(j) == n)) 
	  yield (List(i, j))
	return s.toList 

  }
  
   def test4(primes: Stream[Int], n: Int): List[List[Int]] = {
     println(n)
    val p = primes.takeWhile(_ <= n)
	val s = for (i <- 0 until p.size; j <- i + 1 until p.size; k <- j + 1 until p.size; z <- k + 1 until p.size; 
	if (primes(i) + primes(j) + primes(k) + primes(z) == n)) 
	  yield (List(i, j, k, z))
	return s.toList 

  }
  
  
  def lowCombos(min: Int, s1: Stream[Int], s2: Stream[Int], s3: Stream[Int]): Stream[Int] = {
    // min <= i < j < k
    
    
    
    
    return Stream.Empty
  }
  
  def lowCombos(min: Int, s1: Stream[Int], s2: Stream[Int]): Stream[Int] = {
    
    // min <= i < j
    // is (0, 3) better than (1, 2)

    var i = -1
    var j = 0
    var k = 0
    var l = 0
    var result = Stream.Empty
    while (true) {
    	i = i + 1
    	j = i + 1
    	k = i + 1
    	l = k + 1
    	if (s1(i) + s2(j) <= s1(k) + s2(l)) {
//    		result = result.append((i, j))
    		j = j + 1    		
    	} else {
//    	  result = result.append((k, l))
    	  k = k + 1
    	  l = k + 1
    	}
    	
    }
    
    // (0, 1),
    
//    (0, 1) #:: lowCombos(s.tail, s.tail)
    // consider
    
    Stream.Empty
  }
  
}

