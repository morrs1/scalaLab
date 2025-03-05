
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

  ///////////////////////////////Третье задание//////////////////////////////

  trait Sum {
    def sum(): Int
  }

  trait Prod {
    def prod(): Int
  }

  trait Avg {
    def avg(): Double
  }


  class MyList extends Sum, Prod, Avg {
    private var vector: Vector[Int] = Vector[Int]()

    def add(elem: Int): Unit = {
      vector = vector :+ elem
    }

    def remove(ind: Int): Unit = {
      if (ind < 0 || ind >= vector.length) {
        println(s"Индекс $ind вне диапазона.")
      } else {
        vector = vector.take(ind) ++ vector.drop(ind + 1)
      }
    }

    def get(ind: Int): Int = {
      if (ind >= vector.length) {
        throw new Exception("Индекс за пределами границ массива")
      }
      else {
        vector(ind)
      }
    }

    def printList(): Unit = {
      print("Элементы списка: ")
      vector.foreach(elem => print(elem.toString + " "))
      print("\n")
    }

    override def sum(): Int = {
      vector.sum()
    }

    override def prod(): Int = {
      var prod = 1
      vector.foreach(elem => {
        prod = prod * elem
      })
      prod
    }

    override def avg(): Double = {
      vector.sum / vector.length.toDouble
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
    //Третье задание
    val myList = thirdLab.MyList()
    myList.add(1)
    myList.add(2)
    myList.add(3)
    myList.add(4)
    myList.add(5)
    myList.printList()
    myList.remove(4)
    myList.printList()
    println(myList.sum())
    println(myList.prod())
    println(myList.avg())
  }
}
