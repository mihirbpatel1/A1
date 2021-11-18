import scala.math.sqrt


object promble2 {
  def perfectInt(input: Int) = ((2 to sqrt(input).toInt).collect {case x if input % x == 0 => x + input / x}).sum == input - 1
  
  def main(int: Int): Unit ={
    println(perfectInt(6))
  }
}
