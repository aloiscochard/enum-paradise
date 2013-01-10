package test

import scalax._

object Days extends Enum('Monday, 'Tuesday, 'Wednesday, 'Thursday, 'Friday)

object Test {
  type X = Days.Value

  def xs: Seq[Days.Value] = Days.values

  import Days._
  def f(x: Value) = x match {
    case Monday => 
    case Tuesday => 
    case Wednesday => 
    case Thursday => 
    case Friday => 
  }
}


/*

object Days extends Enum('Monday("MON"), ...)

sealed trait Planet extends Value {
  def mass: Double
  def radius: Double
}

object Planets extends Enum[Planet](
  'Mercury(3.303e+23, 2.4397e6),
  'Venus(4.869e+24, 6.0518e6),
  ...
)
*/
