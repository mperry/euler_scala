
package mperry
import scala.math.BigInt.int2bigInt

/**
 *
 * Starting in the top left corner of a 2x2 grid, there are 6 routes (without
 * backtracking) to the bottom right corner.
 *
 * How many routes are there through a 20x20 grid?
 *
 */
object P015 {

  type Path = List[Pos]
  case class Pos(x: Int, y: Int)

  def p = {
    // an n x n grid is really a (n + 1) x (n + 1) point grid
    val n = 21
    val oracle = 137846528820L
    val x = List(3, n)
    val myList = List(6, oracle)
    val ans = x.map(move(_))
    println(ans)
    assert(ans == myList)
  }

  def move(size: Int): BigInt = {
    val path: Path = Pos(1, 1) :: Nil
    var m = scala.collection.mutable.Map.empty[Pos, BigInt]
    val n = move(path, Pos(size, size), m)
    println(m)
    return n
  }

  def pathX(p: Path): Int = {
    p.head.x
  }

  def pathY(p: Path): Int = {
    p.head.y
  }

  def move(path: Path, f: Pos, m: scala.collection.mutable.Map[Pos, BigInt]): BigInt = {
    val pos = path.head
    val opt = m.get(pos)
    opt match {
      case None =>
      case Some(x) => return x
    }
    var b: BigInt = 0
    if (pos == f) {
      b = BigInt(1)
    } else {
      val x = pathX(path)
      val y = pathY(path)
      // move x and y
      if (x < f.x) {
        b = b + move(Pos(x + 1, y) :: path, f, m)
      }
      if (y < f.y) {
        b = b + move(Pos(x, y + 1) :: path, f, m)
      }
    }
    m.put(pos, b)
    return b

  }

}

