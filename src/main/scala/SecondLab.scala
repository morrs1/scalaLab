class SecondLab {

  abstract class ComputerTech {
    protected var isPower: Boolean
    var powerConsumption: Int

    def run(): Unit

  }

  abstract class ComputerComponent extends ComputerTech {
    def connect(): Unit
  }

  class Monitor extends ComputerTech {
    protected var isPower = true
    var powerConsumption = 100

    override def run(): Unit = {
      if (isPower) {
        println(s"Вывожу изображение на монитор с энергопотреблением в ${powerConsumption}Вт")
      } else {
        println("Монитор не подключен к электросети")
      }
    }
  }


  class KeyBoard extends ComputerTech {
    protected var isPower = true
    var powerConsumption = 10

    override def run(): Unit = {
      if (isPower) {
        println(s"Печатаю сообщения с энергопотреблением в ${powerConsumption}Вт")
      } else {
        println("Клавиатура не подключена к электросети")
      }
    }
  }

  class Audio extends ComputerTech {
    protected var isPower = true
    var powerConsumption = 50

    override def run(): Unit = {
      if (isPower) {
        println(s"Воспроизвожу музыку энергопотреблением в ${powerConsumption}Вт")
      } else {
        println("Аудио система не подключена к электросети")
      }
    }
  }

  class Printer extends ComputerTech {
    protected var isPower = true
    var powerConsumption = 40
    private val colorOfInk = "черные"

    override def run(): Unit = {
      if (isPower) {
        println(s"Печатаю текст на бумаге, используя $colorOfInk чернила с энергопотреблением в ${powerConsumption}Вт")
      } else {
        println("Принтер не подключен к электросети")
      }
    }
  }

  class Computer extends ComputerTech {
    protected var isPower = true
    private val listOfComponents = List[ComputerComponent](Processor(), MotherBoard(), HardDisk()) 
    var powerConsumption: Int = listOfComponents.map(_.powerConsumption).sum()


    override def run(): Unit = {
      if (isPower) {
        listOfComponents.foreach(_.connect())
        println(s"Работаю компьютером с энергопотреблением в ${powerConsumption}Вт")
        listOfComponents.foreach(_.run())
      } else {
        println("Компьютер не подключен к электросети")
      }
    }
  }

  class Processor extends ComputerComponent {
    protected var isPower = true
    var powerConsumption = 90

    override def run(): Unit = {
      if (isPower) {
        println(s"Вычисляю значения с энергопотреблением в ${powerConsumption}Вт")
      } else {
        println("Процессор не подключен к электросети")
      }
    }

    override def connect(): Unit = {
      println("Процессор: Подключаюсь к компьютеру")
    }
  }

  class MotherBoard extends ComputerComponent {
    protected var isPower = true
    var powerConsumption = 10

    override def run(): Unit = {
      if (isPower) {
        println(s"Слежу за состоянием всех компонентов с энергопотреблением в ${powerConsumption}Вт")
      } else {
        println("Мат плата не подключена к электросети")
      }
    }

    override def connect(): Unit = {
      println("Мат плата: Подключаюсь к компьютеру")
    }
  }

  class HardDisk extends ComputerComponent {
    protected var isPower = true
    var powerConsumption = 15

    override def run(): Unit = {
      if (isPower) {
        println(s"Храню информацию с энергопотреблением в ${powerConsumption}Вт")
      } else {
        println("Хард не подключен к электросети")
      }
    }

    override def connect(): Unit = {
      println("Хард: Подключаюсь к компьютеру")
    }
  }

}

object Main2 {
  def main(args: Array[String]): Unit = {
    val secondLab = SecondLab()
    secondLab.Monitor().run()
    secondLab.KeyBoard().run()
    secondLab.Printer().run()
    secondLab.Computer().run()
  }
}






