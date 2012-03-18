package mperry

/*
 * A palindromic number reads the same both ways. The largest palindrome made from 
 * the product of two 2-digit numbers is 9009 = 91 × 99.
 * 
 * Find the largest palindrome made from the product of two 3-digit numbers.
 * 
 */
object P004 {

  def p() = {
    val range = 100 to 999
    val nums = for (i <- range; j <- range if isPalindrome((i * j).toString)) yield (i * j)
    val sorted = nums.sortWith(_ < _)
    val big = sorted.last
    assert(big == 906609)
    println("big = " + big + " list = " + sorted)
  }
  
  def isPalindrome(s: String): Boolean = {
    s.reverse.equals(s)
  }
  
}
