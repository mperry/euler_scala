package mperry.math

object Fibonacci {

    lazy val fib: Stream[Int] = {
    1 #:: 2 #:: fib.zip(fib.tail).map(p => p._1 + p._2)
  }

    lazy val longFib: Stream[Int] = {
      1 #:: 1#:: longFib.zip(longFib.tail).map(p => p._1 + p._2)
    }
    
}