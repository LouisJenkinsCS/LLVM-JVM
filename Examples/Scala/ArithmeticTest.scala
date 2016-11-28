object ArithmeticTest {

  def main(args: Array[String]) {
    val x = 100
    println("X = " + x)
    val y = 1000
    println("Y = " + y)
    val z = 50
    println("Z = " + z)
    val addResult = x + y + z
    val subResult = x - y - z
    val multResult = x * y * z
    val divResult = x / y / z
    val mixedResult = x + y / z
    println("X + Y + Z = " + addResult)
    println("X - Y = Z = " + subResult)
    println("X * Y * Z = " + multResult)
    println("X / Y / Z = " + divResult)
    println("X + Y / Z = " + mixedResult)
  }
}
