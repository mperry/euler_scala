package mperry
import java.util.Calendar

/**
 * You are given the following information, but you may prefer to do some research for 
 * yourself.
 * 
 * 1 Jan 1900 was a Monday.
 * Thirty days has September,
 * April, June and November.
 * All the rest have thirty-one,
 * Saving February alone,
 * Which has twenty-eight, rain or shine.
 * And on leap years, twenty-nine.
 * A leap year occurs on any year evenly divisible by 4, but not on a century unless it 
 * is divisible by 400.
 *
 * How many Sundays fell on the first of the month during the twentieth century 
 * (1 Jan 1901 to 31 Dec 2000)?
 * 
 */
object P019 {
  
  def p = {
	  val s = sundays
	  println(s)
	  assert(s == 171)
  }
  
  def sundays: Int = {
    val c = Calendar.getInstance
    c.set(1901, Calendar.JANUARY, 1)
    var result = 0
    while (c.get(Calendar.YEAR) <= 2000) {
      if (c.get(Calendar.DAY_OF_WEEK) == Calendar.SUNDAY) {
        result += 1
      }
      c.add(Calendar.MONTH, 1)
    }
    return result
  }
  
}
