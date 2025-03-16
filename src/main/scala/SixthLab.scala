import java.util.stream.IntStream

class SixthLab {
  private val a: Int = 5

  def subWord(str: String, sep: String): () => String = {
    var array: Array[String] = str.split(sep)

    def innerSubWord(): String = {
      if(array.length == 0){
        return "nil"
      }
      val head = array.head
      array = array.tail
      head
    }

    innerSubWord
  }

}

object Main6 {
  def main(args: Array[String]): Unit = {
    val sixthLab = SixthLab()
    val subWord = sixthLab.subWord("1:2:3:4:5:6", ":")
    IntStream.range(0, 8).forEach(_=>println(subWord.apply()))
  }
}