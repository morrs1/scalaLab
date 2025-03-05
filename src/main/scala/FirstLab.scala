import scala.util.Random

class FirstLab {

  def penultimateRec[T](list: List[T]): T = {
    list match {
      case p :: _ :: Nil => return p
      case _ => Nil
    }
    penultimateRec(list.tail)
  }

  def penultimateCycle[T](list: List[T]): T = {
    val len = lenOfListCycle(list)
    var secondCopyOfList = list
    for (_ <- 1 to len - 2) {
      secondCopyOfList = secondCopyOfList.tail
    }
    secondCopyOfList.head
  }

  def lenOfListCycle[T](list: List[T]): Int = {
    var len = 0
    list.foreach(el => len += 1)
    len
  }

  def lenOfListRec[T](list: List[T]): Int = {
    list match {
      case Nil => 0
      case _ :: tail => 1 + lenOfListRec(list.tail)
    }
  }

  def compressCycle[T](list: List[T]): List[T] = {
    var copyOfList = List[T]()
    for (i <- 0 until lenOfListCycle(list) - 1) {
      if (list(i) == list(i + 1)) {}
      else {
        copyOfList = copyOfList :+ list(i)
      }
    }
    copyOfList :+ list.last
  }

  def compressRec[T](list: List[T]): List[T] = {
    def compressHelper(remaining: List[T], result: List[T]): List[T] = remaining match {
      case Nil => result.reverse
      case head :: tail =>
        val updatedResult = if (result.isEmpty || head != result.head) head :: result else result
        compressHelper(tail, updatedResult)
    }

    compressHelper(list, Nil)
  }

  def insertCycle[T](el: T, ind: Int, list: List[T]): List[T] = {
    (list.take(ind) :+ el) ++ list.drop(ind)
  }

  def insertRec[T](el: T, ind: Int, list: List[T]): List[T] = {
    def insertRec(currentList: List[T], currentIndex: Int): List[T] = {
      currentList match {
        case Nil =>
          if (currentIndex == ind) List(el) else Nil
        case head :: tail =>
          if (currentIndex == ind) el :: head :: tail
          else head :: insertRec(tail, currentIndex + 1)
      }
    }

    insertRec(list, 0)
  }

  def lottoCycle(n: Int, m: Int): List[Int] = {
    var newList = List[Int]()
    val random = Random()
    for (_ <- 0 until n) {
      newList = random.nextInt(m) :: newList
    }
    newList
  }

  def lottoRec(n: Int, m: Int): List[Int] = {
    def generateNumbers(count: Int, random: Random): List[Int] = {
      if (count <= 0) {
        List()
      } else {
        random.nextInt(m) :: generateNumbers(count - 1, random)
      }
    }

    val random = new Random()
    generateNumbers(n, random)
  }

  def or(first: Boolean, second: Boolean): Boolean = {
    if (first || second) true else false
  }

  def and(first: Boolean, second: Boolean): Boolean = {
    if (first && second) true else false
  }

  def chordMethod(f: Double => Double, a: Double, b: Double, epsilon: Double = 1e-6, maxIter: Int = 100): Double = {
    require(f(a) * f(b) <= 0, "Интервал [a, b] должен содержать корень (f(a) и f(b) разных знаков)")
    var xPrev = a
    var xCurr = b
    var iter = 0
    while (iter < maxIter) {
      val fPrev = f(xPrev)
      val fCurr = f(xCurr)
      val xNext = xCurr - fCurr * (xCurr - xPrev) / (fCurr - fPrev)
      if (math.abs(xNext - xCurr) < epsilon) return xNext
      xPrev = xCurr
      xCurr = xNext
      iter += 1
    }
    xCurr
  }

  def chordMethodRec(f: Double => Double, xPrev: Double, xCurr: Double, epsilon: Double = 1e-6, maxIter: Int = 100, iter: Int = 0): Double = {
    if (iter >= maxIter) xCurr
    else {
      val fPrev = f(xPrev)
      val fCurr = f(xCurr)
      val xNext = xCurr - fCurr * (xCurr - xPrev) / (fCurr - fPrev)
      if (math.abs(xNext - xCurr) < epsilon) xNext
      else {
        chordMethodRec(f, xCurr, xNext, epsilon, maxIter, iter + 1)
      }
    }
  }

  //  def isPrimeCycle(number: Int): Boolean = {
  //    val divisors = (1 to Math.sqrt(number).toInt).filter(number % _ == 0).toList
  //    if (lenOfListCycle(divisors) == 1) true else false
  //  }
  //
  //  def isPrimeRec(number: Int): Boolean = {
  //    def isPrimeRec(divisor: Int): Boolean = {
  //      if (divisor * divisor > number) {
  //        true
  //      } else if (number % divisor == 0) {
  //        false
  //      } else {
  //        isPrimeRec(divisor + 1)
  //      }
  //    }
  //
  //    if (number <= 1) {
  //      false
  //    } else {
  //      isPrimeRec(2)
  //    }
  //  }
}


object Main {
  def main(args: Array[String]): Unit = {
    val list = List(1, 1, 3, 3, 5, 5, 0, 10, 10, 10)
    val firstLab = FirstLab()
    //Первое задание
    printf("Предпоследний элемент списка с помощью рекурсии: %d\n", firstLab.penultimateRec(list))
    printf("Предпоследний элемент списка с помощью цикла: %d\n", firstLab.penultimateCycle(list))
    //Второе задание
    printf("Длина списка с помощью цикла: %s\n", firstLab.lenOfListCycle(list))
    printf("Длина списка с помощью рекурсии: %s\n", firstLab.lenOfListRec(list))
    //Третье задание
    printf("Сжатие списка с помощью цикла: %s\n", firstLab.compressCycle(list))
    printf("Сжатие списка с помощью рекурсии: %s\n", firstLab.compressRec(list))
    //Четвертое задание
    printf("Вставка в список с помощью рекурсии: %s\n", firstLab.insertRec(4, 3, list))
    printf("Вставка в список с помощью цикла: %s\n", firstLab.insertCycle(4, 3, list))
    //Пятое задание
    printf("Генерация массива с помощью цикла: %s\n", firstLab.lottoCycle(6, 49))
    printf("Генерация массива с помощью рекурсии: %s\n", firstLab.lottoRec(6, 49))
    //Шестое задание
    //    printf("Является ли число простым с помощью цикла: %s\n", firstLab.isPrimeCycle(241))
    //    printf("Является ли число простым с помощью рекурсии: %s\n", firstLab.isPrimeRec(241))
    printf("OR: %s\n", firstLab.or(true, false))
    printf("AND: %s\n", firstLab.and(true, false))

    //Седьмое задание
    val f = (x: Double) => math.pow(x, 3) + 18 * x - 83
    val root = firstLab.chordMethod(f, a = 3.0, b = 4.0)
    println(f"Приближенный корень: $root%.6f")

    //Седьмое задание с помощью рекурсии
    require(f(3.0) * f(4.0) <= 0, "Интервал [a, b] должен содержать корень")
    val rootRec = firstLab.chordMethodRec(f, xPrev = 3.0, xCurr = 4.0)
    println(f"Приближенный корень с помощью рекрсии: $rootRec%.6f")
  }
}
