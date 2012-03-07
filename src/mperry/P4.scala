package mperry

object P4 {

  def p() = {
    
    val range = 100 to 999
    val nums = for (i <- range; j <- range if isPalindrome((i * j).toString)) yield (i * j)
    val big = nums.max
    println(big)
  }
  
  def isPalindrome(s: String): Boolean = {
    s.reverse.equals(s)
  }
  
}
