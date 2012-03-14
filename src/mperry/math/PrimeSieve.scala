package mperry.math

object PrimeSieve {
  
  /**
   * This is fast, it reuses the one stream, only checking up to root(n)
   */
  val lazyValSieve: Stream[Int] = 2 #:: Stream.from(3).filter(
      i => lazyValSieve.takeWhile(j => j * j <= i).forall(i % _ > 0))
  
}
