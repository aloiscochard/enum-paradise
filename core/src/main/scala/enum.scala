package scalax

object Days extends Enum(Monday, Tuesday, Wednesday, Thursday, Friday)

object DaysDemo {
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
object Days extends Enum(Monday("MON"), ...)
*/

