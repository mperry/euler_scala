package mperry

object P1 {

	def problem() = {
	  val v = (1 until 1000).filter(n => n % 3 == 0 || n % 5 == 0).foldLeft (0) (_ + _)
	  println("p1 = " + v);
	}
}