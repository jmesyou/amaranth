package samples

object HelloWorld {
  def main(args: Array[String]): Unit = {
    println("Hello World")
  }

  def checksum[T](data: Array[T]): Int = {
    var sum = 0
    var idx = 0
    while (idx < data.length) {
      sum += data(idx).##
      idx += 1
    }
    sum
  }
}
