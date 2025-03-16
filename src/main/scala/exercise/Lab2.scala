package exercise

import exercise.Lab2.Expr
import exercise.Lab2.Expr.{Literal, Multiply}

object Lab2 extends App:

  // Task 3.a
  val f: Int => String = _ match
    case n if n >= 0 => "positive"
    case n if n < 0 => "negative"

  println(f(5))  // positive
  println(f(0))  // positive
  println(f(-5))  // negative

  def f2(x: Int): String = x match
    case x if x >= 0 => "positive"
    case x if x < 0 => "negative"

  println(f2(5)) // positive
  println(f2(0)) // positive
  println(f2(-5)) // negative
  println()


  // Task 3.b
  val neg: (String => Boolean) => (String => Boolean) =
    predicate => str => !predicate(str)

  def neg2(pred: String => Boolean): String => Boolean = s => !pred(s)

  val empty: String => Boolean = _ == ""

  val notEmpty = neg(empty)
  println(notEmpty("foo"))
  println(notEmpty(""))

  val notEmpty2 = neg2(empty)
  println(notEmpty2("foo"))
  println(notEmpty2(""))
  println()


  // Task 3.c
  def neg2Generics[X](pred: X => Boolean): X => Boolean = x => !pred(x)

  val emptyGenenericsString: String => Boolean = _ == ""

  val notEmptyGenenericsString = neg(empty)
  println(notEmpty("foo"))
  println(notEmpty(""))

  val emptyGenericsInt: Int => Boolean = _ > 0

  val notEmptyGenericsInt = neg(empty)
  println(notEmpty("foo"))
  println(notEmpty(""))
  println()


  // Task 4
  val p1: Int => Int => Int => Boolean =
    x => y => z =>
      x <= y && y == z

  val p2: (Int, Int, Int) => Boolean = (x, y, z) => (x, y, z) match
    case (x, y, z) if x <= y && y == z => true
    case _ => false

  def p3(x: Int)(y: Int)(z: Int): Boolean =
    x <= y && y == z

  def p4(x: Int,y: Int,z: Int): Boolean =
    x <= y && y == z

  println(p1(1)(1)(1)) // true
  println(p1(2)(1)(2)) // false
  println(p2(1, 1, 1)) // true
  println(p2(2, 1, 2)) // false
  println(p3(1)(1)(1)) // true
  println(p3(2)(1)(2)) // false
  println(p4(0, 1, 1)) // true
  println(p4(2, 1, 2)) // false


  // Task 5
  def compose(f: Int => Int, g: Int => Int): Int => Int =
    x => f(g(x))

  def composeGenerics[X,Y,Z](f: Y => Z, g: X => Y): X => Z =
    x => f(g(x))

  val times: Int => Int = _ * 2
  val toDouble: Int => Double = x => x

  println(compose(_ - 1, _ * 2)(5)) // 9
  println(composeGenerics(toDouble , times)(5))  // 10.0
  println()


  // Task 6
  def composeThree[A,B,C,D](f: C => D, g: B => C, h: A => B): A => D =
    composeGenerics(f, composeGenerics(g, h))
  //println(composeThree(_ + "!", _.toString, _ * 2)(3))


  // Task 7
  def power(base: Double, exponent: Int): Double = exponent match
    case 0 => 1
    case 1 => base
    case _ => base * power(base, exponent - 1)

  def powerTail(base: Double, exponent: Int): Double =
    @annotation.tailrec
    def _powerTail(base: Double, exponent: Int, acc: Double): Double = exponent match
      case 0 => acc
      case _ => _powerTail(base, exponent - 1, acc * base)
    _powerTail(base, exponent, 1)

  println((power(2, 3), power(5, 2)))  // (8.0,25.0)
  println((powerTail(2, 3), powerTail(5, 2)))  // (8.0,25.0)
  println()


  // Task 8
  def reverseNumber(n: Int): Int =
    @annotation.tailrec
    def _reverseNumber(remaining: Int, acc: Int): Int =
      if (remaining <= 0) acc
      else _reverseNumber(remaining / 10, acc * 10 + (remaining % 10))
    _reverseNumber(n, 0)

  println(reverseNumber(1234)) // 4321
  println()


  // Task 9
  enum Expr:
    case Literal(const: Int)
    case Add(value1: Expr, value2: Expr)
    case Multiply(value1: Expr, value2: Expr)

  def evaluate(expr: Expr): Int = expr match
    case Expr.Literal(value) => value
    case Expr.Add(value1, value2) => evaluate(value1) + evaluate(value2)
    case Expr.Multiply(value1, value2) => evaluate(value1) * evaluate(value2)

  def show(expr: Expr): String = expr match
    case Expr.Literal(value) => value.toString
    case Expr.Add(value1, value2) => "(" + show(value1) + " + " + show(value2) + ")"
    case Expr.Multiply(value1, value2) => "(" + show(value1) + " * " + show(value2) + ")"

  val expr2 = Expr.Multiply(Expr.Literal(2), Expr.Add(Expr.Literal(3), Expr.Literal(4)))

  println(evaluate(expr2))  // 14
  println(show(expr2))  // (2 * (3 + 4))




