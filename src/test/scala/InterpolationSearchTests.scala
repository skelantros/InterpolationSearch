import org.scalatest.funsuite.AnyFunSuite
import InterpolationSearch._
import org.scalatest.matchers.should.Matchers
import TestingUtils._

class InterpolationSearchTests extends AnyFunSuite {
  test("Empty array") {
    assert(interpolateSearch(Array.empty[Int])(0) == (None, 1))
  }
  test("Random arrays") {
    for(i <- 0 until 100) {
      val size = number(10, 1000)
      val n1 = number(0, 1000)
      val n2 = number(0, 1000)
      val from = math.min(n1, n2)
      val until = math.max(n1, n2) + 1
      val x = number(from, until)
      val arr = new Array[Int](size)
      fillSortArray(arr, from, until)
      val (res, iters) = interpolateSearch(arr)(x)
      res match {
        case Some(idx) => assert(arr(idx) == x, s"\nElement $x, found pos $res, but arr[$res] = ${arr(idx)}")
        case None => assert(!arr.contains(x), s"\nElement $x, pos is not found, but array contains $x")
      }
    }
  }
}
