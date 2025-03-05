class ThirdLab {
  ///////////////////////////////Первое задание//////////////////////////////
  trait PowerByTwo {
    def powerByTwo(x: Int): Int = x * x
  }

  trait PowerByThree {
    def powerByThree(x: Int): Int = x * x * x
  }

  trait PowerByFour {
    def powerByFour(x: Int): Int = x * x * x * x
  }

  trait PowerByFive {
    def powerByFive(x: Int): Int = x * x * x * x * x
  }

  object PowNumber extends PowerByTwo, PowerByThree, PowerByFour, PowerByFive {}
  ///////////////////////////////Второе задание//////////////////////////////

  trait Addable[T] {
    def add(elem: T): Unit
  }

  trait Removable[T] {
    def remove(): Unit
  }

  trait Printable[T] {
    def printQ(): Unit
  }

  class Queue[T] extends Addable[T], Removable[T], Printable[T] {
    var list: List[T] = List[T]()

    override def add(elem: T): Unit = {
      list = elem :: list
    }

    override def remove(): Unit = {
      list = list.tail
    }

    override def printQ(): Unit = {
      print("Элементы очереди: ")
      list.foreach(elem => print(elem.toString + " "))
      print("\n")
    }
  }

}

object Main3 {
  def main(args: Array[String]): Unit = {
    val thirdLab = ThirdLab()
    //Первое задание
    println(thirdLab.PowNumber.powerByTwo(2))
    println(thirdLab.PowNumber.powerByThree(2))
    println(thirdLab.PowNumber.powerByFour(2))
    println(thirdLab.PowNumber.powerByFive(2))
    //Второе задание
    val queue = thirdLab.Queue[Int]()
    queue.add(5)
    queue.add(6)
    queue.printQ()
    queue.remove()
    queue.printQ()
    queue.add(22)
  }
}
