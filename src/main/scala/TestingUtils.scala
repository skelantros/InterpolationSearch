import scala.util.Random
import scala.util.Sorting.quickSort

object TestingUtils {
  private val rand = new Random()
  // Функция, возвращающая случайное целое число в интервале [from; until)
  def number(from: Int, until: Int) = rand.nextInt(until - from) + from
  // Функция, заполняющая массив случайными значениями в заданном интервале и сортирующая массив
  def fillSortArray(arr: Array[Int], from: Int, until: Int): Unit = {
    for(i <- arr.indices) arr(i) = number(from, until)
    quickSort(arr)
  }
}
