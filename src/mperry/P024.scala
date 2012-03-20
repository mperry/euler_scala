package mperry

/**
 *
 * A permutation is an ordered arrangement of objects. For example, 3124 is one possible
 * permutation of the digits 1, 2, 3 and 4. If all of the permutations are listed
 * numerically or alphabetically, we call it lexicographic order. The lexicographic
 * permutations of 0, 1 and 2 are:
 *
 * 012   021   102   120   201   210
 *
 * What is the millionth lexicographic permutation of the digits
 * 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?
 *
 */
object P024 {

  val size = Math.pow(10, 6).toInt

  def p = {
    //    bruteForce
    lazySolution
  }

  def bruteForce = {
    val m = perm(0, 2)
    println(m)
//    val n = perm(0, 9)
//    println(n(size))

  }

  def lazySolution = {
    val w = perm((0 to 9).toList, (x: List[List[Int]]) => x.length >= size)
    val mill = w(size - 1)
    val s = mill.fold("")(_ + _.toString)
    println("s = " + s)
    assert(s == "2783915460")

  }

  /**
   * produces all permutations
   */
  def perm(low: Int, high: Int): Stream[String] = {
    val perms = (low to high).permutations.toStream
    println(perms)
    val list = perms.map(x => x.fold("")(_.toString + _.toString())).map(_.toString);
    list.sortWith(_ < _)
    //	list
  }

  def perm(list: List[Int], stopCondition: (List[List[Int]] => Boolean)): List[List[Int]] = {
    if (list.length <= 1) {
      return List(list)
    }

    var i = 0
    var result: List[List[Int]] = Nil
    while (i < list.length) {
      val pair = list.splitAt(i)
      val newList: List[Int] = pair._1 ++ pair._2.tail
      val e = list(i)
      result = result ++ perm(newList).map(e :: _)
      i = i + 1
      if (stopCondition(result)) {
        return result
      }
    }
    result
  }

  /**
   * No stopping condition, produce all permutations
   */
  def perm(list: List[Int]): List[List[Int]] = {
    perm(list, (x: List[List[Int]]) => false)
  }

}
