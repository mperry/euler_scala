package mperry
import mperry.math.Fibonacci

/**
 * The Fibonacci sequence is defined by the recurrence relation:
 * Fn = Fn1 + Fn2, where F1 = 1 and F2 = 1.
 * 
 * Hence the first 12 terms will be:
 * 
 * F1 = 1 F2 = 1 F3 = 2 F4 = 3 F5 = 5 F6 = 8 F7 = 13 F8 = 21 F9 = 34
 * F10 = 55 F11 = 89 F12 = 144
 * 
 * The 12th term, F12, is the first term to contain three digits.
 * 
 * What is the first term in the Fibonacci sequence to contain 1000 digits?
 * 
 */

object P025 {
  
  def p = {
    def f = Fibonacci.longFib
    def digits = 3
    def high = BigInt(10).pow(digits - 1)
    def e = f.dropWhile(_ < high).head
    println(e)
    assert(true)
    
  }
  
}
