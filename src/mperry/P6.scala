package mperry

object P6 {

	/*  
	The sum of the squares of the first ten natural numbers is,
	12 + 22 + ... + 102 = 385
	
	The square of the sum of the first ten natural numbers is,
	(1 + 2 + ... + 10)2 = 552 = 3025
	
	Hence the difference between the sum of the squares of the first ten natural numbers and 
	the square of the sum is 3025 - 385 = 2640.
	
	Find the difference between the sum of the squares of the first one hundred natural 
	numbers and the square of the sum.
	*/

  
  def p = {
    
    val s = sumSquares(10)
    val r = squareSum(10)
    val d = diff(100)
    println("s = " + s + " r = " + r + " diff = " + d)
  }
  
  def diff(n: Int):Int = { 
    squareSum(n) - sumSquares(n)
  }
  
  def sumSquares(n: Int): Int = {
    val v = (1 to n).map(n => n * n)
    val r = v.foldLeft (0) (_ + _)
    r
  }

  def squareSum(n: Int): Int = {
    val sum = (1 to n).foldLeft (0) (_ + _)
    sum * sum
  }
  
}