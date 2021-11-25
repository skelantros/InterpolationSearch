import TestingUtils._
import InterpolationSearch._

object Main extends App {
  // Функция для тестирования алгоритма интерполяционного поиска
  // func: функция расчета интервала, в котором будут находиться элементы массива, на основе его размера
  // sizes: множество размеров, на котором будет тестироваться функция
  // attemps: число измерений на каждом размере
  def benchmarkOps(func: Int => (Int, Int), sizes: Seq[Int], attempts: Int): Unit = {
    println(s"size\titers")
    for {
      s <- sizes
      (from, until) = func(s)
      arr = new Array[Int](s)
      results = for {
        _ <- 0 until attempts
        _ = fillSortArray(arr, from, until)
        toFind = number(from, until)
        (res, iters) = interpolateSearch(arr)(toFind)
      } yield iters
      result = results.sum.toDouble / results.length
    } println(s"$s\t$result")
  }

  // Множество размеров: 10, 10^2, ..., 10^8
  val sizes = Seq.tabulate(8)(i => math.pow(10, i + 1).toInt)
  // Вычислительный эксперимент
  benchmarkOps(s => (0, s / 2), sizes, 50)
  // Вычислительный эксперимент на данных удвоенного размера
  benchmarkOps(s => (0, s / 2), sizes.map(_ * 2), 50)
}
