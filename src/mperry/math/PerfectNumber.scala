package mperry.math

object PerfectNumber extends Enumeration {

  type PerfectNumber = Value
  val PERFECT, ABUNDANT, DEFICIENT = Value

  def isPerfect(i: Int): PerfectNumber = {
    val s = Factors.factors(i).sum  
    if (s == i) {
      PERFECT
    } else if (s < i) {
      DEFICIENT
    } else {
      ABUNDANT
    }
  }
  
}
