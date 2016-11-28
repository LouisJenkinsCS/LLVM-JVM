object ConditionalTest {

  def main(args: Array[String]) {
    var x = 0
    var i = 0
    while (i < 100) {
      println("Idx: " + i + ";Val: " + x)
      x += 1
      i += 1
    }
  }
}
