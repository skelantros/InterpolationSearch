// Наличие Metric[T] для типа T означает, что между двумя объектами типа T можно вычислить вещественную "разность"
// Кроме того, наличие "метрики" позволяет ввести отношение порядка на множестве значений типа T (выражается как Ordering[T])
// Эти свойства понадобятся нам в алгоритме интерполяционного поиска
// P.S. Данная "метрика" не является метрикой в математическом смысле слова, т.к. математическая метрика - это неотрицательная функция
// Название Metric используется исключительно для удобства
trait Metric[A] extends Ordering[A] {
  def measure(x: A, y: A): Double
  override def compare(x: A, y: A): Int = {
    val diff = measure(x, y)
    if(diff < 0) -1
    else if(diff > 0) 1
    else 0
  }
}

object Metric {
  def apply[A : Metric]: Metric[A] = implicitly
  // Введение стандартных "метрик" для целых чисел и чисел с плавающей точкой
  implicit val int: Metric[Int] = new Metric[Int] {
    override def measure(x: Int, y: Int): Double = (x - y).toDouble
  }
  implicit val double: Metric[Double] = new Metric[Double] {
    override def measure(x: Double, y: Double): Double = x - y
  }

  // Операторы для объектов типа A с введенной "метрикой". Нужны исключительно для удобства записи
  implicit class MetricOps[A : Metric](x: A) {
    def |-|(y: A): Double = Metric[A].measure(x, y)
    def <(y: A): Boolean = Metric[A].compare(x, y) < 0
    def >(y: A): Boolean = Metric[A].compare(x, y) > 0
    def >=(y: A): Boolean = !(x < y)
    def <=(y: A): Boolean = !(x > y)
  }
}