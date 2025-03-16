class FourthLab {

  def f(x: Int): List[Int] = x match {
    case 0 => List[Int](0)
    case 1 => List[Int](1)
    case 2 => List[Int](0, 2)
    case 3 => List[Int](2)
    case _ => throw new IllegalArgumentException(s"Недопустимое значение x: $x")
  }

  def getCentury(year: Int): Int = year match {
    case y if y > 0 => ((y - 1) / 100) + 1
    case _ => throw new IllegalArgumentException("Год должен быть положительным")
  }


}

object Main4 {
  def main(args: Array[String]): Unit = {
    val fourthLab = FourthLab()
    fourthLab.f(0).foreach(println(_))
    fourthLab.f(1).foreach(println(_))
    fourthLab.f(3).foreach(println(_))
    //Второе задание
    println("Второе задание")
    println(fourthLab.getCentury(1902))
    println(fourthLab.getCentury(2405))
    println(fourthLab.getCentury(2001))
    println(fourthLab.getCentury(2000))

  }
}
