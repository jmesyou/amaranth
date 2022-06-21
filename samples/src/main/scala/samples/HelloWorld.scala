package samples

object HelloWorld {
  def main(args: Array[String]): Unit = {
    println("Hello World")
  }

  def checksum[T](data: Array[T]): Int = {
    var sum = 0
    var index = 0
    while (index < data.length) {
      sum += data(index).##
      index += 1
    }
    sum
  }
}
