package mperry

object P7 {

  /*
   * By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that 
   * the 6th prime is 13.
   *
   * What is the 10 001st prime number?
   * 
   */
  
  def p = {
  
	 
    val v = sieveWithSize(2::Nil, 2, 10001)

    println(v.head)
	
      
    
  }
  
  
   
  def sieveWithMax(a: List[Int], current: Int, max: Int): List[Int] = {
    if (current % 100 == 0) {
    	println(current, max)  
    }
    
    if (current > max) a
    else if (a.forall(x => current % x != 0)) sieveWithMax(current::a, current + 1, max)
    else sieveWithMax(a, current + 1, max)
  }

    def sieveWithSize(a: List[Int], current: Int, max: Int): List[Int] = {
    if (a.size >= max) a
    else if (a.forall(x => current % x != 0)) sieveWithSize(current::a, current + 1, max)
    else sieveWithSize(a, current + 1, max)
  }

    // incomplete
  def sieveWithStop(a: List[Int], current: Int, max: Int, f: Int => Int): List[Int] = {
    if (a.size >= max) a
    else if (a.forall(x => current % x != 0)) sieveWithStop(current::a, current + 1, max, f)
    else sieveWithStop(a, current + 1, max, f)
  }

  
  
}