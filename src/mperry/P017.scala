package mperry

/**
 * 
 * If the numbers 1 to 5 are written out in words: one, two, three, four, five, then 
 * there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
 * 
 * If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, 
 * how many letters would be used?
 * 
 * NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) 
 * contains 23 letters and 115 (one hundred and fifteen) contains 20 letters. The use 
 * of "and" when writing out numbers is in compliance with British usage.
 * 
 */
object P017 {

  def p = {
    
    def total = (1 to 1000).map(count(_)).sum
    def i = 441
    def w = digits3(i)
    def s = count(i)
    println("i = " + i + " count = " + s + " words = " + w)
    println("total = " + total)
    assert(total == 21123)
  }
  
  def count(i: Int): Int = {
//    'a' != 'b'
    digits3(i).filter(x => x != ' ' && x != '-').size
  }
  
  def digits3(i: Int): String = {
    if (i < 10) return digits1(i)
    if (i < 100) return digits2(i)
    def h = i.toString.head
    def d2 = digits2(i.toString.tail.toInt)
    def d3 = digits1(h.getNumericValue) + " hundred"
    if (d2.size > 0) {
      d3 + " and " + d2 
    } else {
      d3 
    }
    
  }
  
  def digits2(i: Int): String = {
    
    val h = i.toString.head.getNumericValue
    if (i < 10) digits1(i)
    else {
      h match {
        case 1 => teens(i)
        case _ =>
          def a = h match {
        case 2 => "twenty " 
        case 3 => "thirty"
        case 4 => "forty"
        case 5 => "fifty"
        case 6 => "sixty"
        case 7 => "seventy"
        case 8 => "eighty"
        case 9 => "ninety"
        case _ => ""    
          } 
          a + " " + digits1(i.toString.reverse.head.getNumericValue)
        
      }
    }
    
    
  }
  
  def teens(i: Int): String = {
    i match {
      case 10 => "ten"
      case 11 => "eleven"
      case 12 => "twelve"
      case 13 => "thirteen"
      case 14 => "fourteen"
      case 15 => "fifteen" 
      case 16 => "sixteen"
      case 17 => "seventeen"
      case 18 => "eighteen"
      case 19 => "nineteen"
        
    }
  }
  def digits1(i: Int): String = {
    i match {
      case 0 => ""
      case 1 => "one"
      case 2 => "two"
      case 3 => "three"
      case 4 => "four"
      case 5 => "five"
      case 6 => "six"
      case 7 => "seven"
      case 8 => "eight"
      case 9 => "nine"
      case _ => ""
    }
  }
  
}
