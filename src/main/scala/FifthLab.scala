class FifthLab {

  case class Circle(radius: Double)

  case class Rectangle(a: Double, b: Double)

  case class Square(a: Double)

  trait SPRectangle {
    def sRectangle(rec: Rectangle): Double

    def pRectangle(rec: Rectangle): Double
  }

  trait SPCircle {
    def sCircle(circle: Circle): Double

    def pCircle(circle: Circle): Double
  }

  trait SPSquare {
    def sSquare(square: Square): Double

    def pSquare(square: Square): Double
  }

  class SPShape extends SPRectangle with SPSquare with SPCircle {

    override def sRectangle(rec: Rectangle): Double = {
      rec.a * rec.b
    }

    override def pRectangle(rec: Rectangle): Double = {
      2 * (rec.a + rec.b)
    }

    override def sSquare(square: Square): Double = {
      square.a * square.a
    }

    override def pSquare(square: Square): Double = {
      square.a * 4
    }

    override def sCircle(circle: Circle): Double = {
      Math.PI * circle.radius * circle.radius
    }

    override def pCircle(circle: Circle): Double = {
      2 * Math.PI * circle.radius
    }
  }

}

object Main5 {
  def main(args: Array[String]): Unit = {
    val fifthLab = new FifthLab()
    val square = fifthLab.Square(4)
    val circle = fifthLab.Circle(5)
    val rectangle = fifthLab.Rectangle(4, 5)
    val shapes = new fifthLab.SPShape()

    println("Площадь квадрата: " + shapes.sSquare(square))
    println("Площадь круга: " + shapes.sCircle(circle))
    println("Площадь прямоугольника: " + shapes.sRectangle(rectangle))

    println("Периметр квадрата: " + shapes.pSquare(square))
    println("Периметр круга: " + shapes.pCircle(circle))
    println("Периметр прямоугольника: " + shapes.pRectangle(rectangle))
  }
}
