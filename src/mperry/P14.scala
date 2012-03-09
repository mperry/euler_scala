package mperry

/**
 * 
 * The following iterative sequence is defined for the set of positive integers:
 * 
 * n -> n/2 (n is even)
 * n -> 3n + 1 (n is odd)
 * 
 * Using the rule above and starting with 13, we generate the following sequence:
 * 13 -> 40 -> 20 -> 10 -> 5 -> 16 -> 8 -> 4 -> 2 -> 1
 * 
 * It can be seen that this sequence (starting at 13 and finishing at 1) contains 
 * 10 terms. Although it has not been proved yet (Collatz Problem), it is thought 
 * that all starting numbers finish at 1.
 * 
 * Which starting number, under one million, produces the longest chain?
 * 
 * NOTE: Once the chain starts the terms are allowed to go above one million.
 * 
 */
object P14 {

  def p = {
    
    val exp = 6
    val high = math.pow(10, exp).toInt
//    val high = 5
    val chains = (1 until high).map(collatz(_))
    val sizes = chains.map(_.size)
    val max = sizes.fold(0) (_ max _)
    val index = sizes.findIndexOf(_ == max)
    val num = index + 1
    
    println("num = " + num + " max = " +  max + " index = " + index)
    
    
    
    val v = collatz(13)
//    println(v)
  }
  
  def collatz(i: BigInt): List[BigInt] = {
    if (i == 1) {
      List(i)
    } else {
      if (i % 2 == 0) {
        i :: collatz(i / 2)
      } else {
        i :: collatz(3 * i + 1)
      }
      
    }
  }
  
}