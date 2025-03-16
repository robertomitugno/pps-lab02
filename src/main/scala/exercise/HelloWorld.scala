package exercise

object HelloWorld extends App:
  println("Hello, Scala") // Hello, Scala
    
  def divide(x: Double, y: Double): Double = x / y

  def divideCurried(x: Double)(y:Double): Double = x / y

  println(divide(5, 5)) // 1.0

  println(divide(5, 7)) // 0.7142857142857143
  
  println(divideCurried(5)(5)) // 1.0
  
  println(divideCurried(5)(7)) // 0.7142857142857143
  
  println(divideCurried(5)(0)) // Infinity
  
