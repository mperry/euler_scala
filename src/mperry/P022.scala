
package mperry
import scala.io.Source

/**
 *
 * Using names.txt (right click and 'Save Link/Target As...'), a 46K text file containing
 * over five-thousand first names, begin by sorting it into alphabetical order. Then working
 * out the alphabetical value for each name, multiply this value by its alphabetical position
 * in the list to obtain a name score.
 *
 * For example, when the list is sorted into alphabetical order, COLIN, which is worth
 * 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN would obtain a score
 * of 938 × 53 = 49714.
 *
 * What is the total of all the name scores in the file?
 *
 */
object P022 {

  val fileName = "names.txt"
  val firstChar = 'A'
  val base = firstChar.intValue()
  val colinIndex = 938 - 1

  def p = {
    assert(value("COLIN", base) == 53)
    val list = Source.fromFile(fileName).mkString.split(",").toList.map(_.replaceAll("\"", "")).sort(_ < _)
    val scores = list.map(value(_, base))
    val weighted = (1 to scores.length).map(x => scores(x - 1) * x)
    val sum = weighted.sum
    assert(weighted(colinIndex) == 49714)
    println("sum = " + sum)
    assert(sum == 871198282)
  }

  def value(s: String, base: Int): Int = {
    s.toUpperCase.map(x => x.intValue - base + 1).fold(0)(_ + _)
  }

}
