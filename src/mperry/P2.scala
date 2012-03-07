package mperry

object P2 {

  def p2() = {
    val r = fib.takeWhile(_ <= 4000000).filter(n => n % 2 == 0).foldLeft (0) (_+_)
//    val r2 = fib.take(20)
//    println("fib = " + r2.print)
    println("fib = " + r)
  }
  
  lazy val fib: Stream[Int] = {
    1 #:: 2 #:: fib.zip(fib.tail).map(p => p._1 + p._2)
  }
  
}