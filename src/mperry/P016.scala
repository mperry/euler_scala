package mperry

/**
 * 2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
 * 
 * What is the sum of the digits of the number 2^1000?
 * 
 */
object P016 {

  def p = {
  
    val exp = 1000
    val num = BigInt(2).pow(exp)
    val total = sum(num)
    println("num = " + num)
    println("total = " + total)
    assert(total == 1366)
  }
  
  def sum(n: BigInt): BigInt = {
    val d = n.toString.map(_.getNumericValue)
    d.fold(0) (_+_)
  }
  
}
