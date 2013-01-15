package scalax

sealed trait Planet extends Value

object Planets extends EnumOf[Planet](
  Mercury,
  Venus
)

object PlanetsDemo {
  import Planets._

  implicitly[Value =:= Planet]

  def f(x: Planet) = x match {
    case Mercury => 
    case Venus => 
  }
}


/*
sealed trait Planet extends Value {
  def mass: Double
  def radius: Double
}

object Planets extends Enum[Planet](
  Mercury(3.303e+23, 2.4397e6),
  Venus(4.869e+24, 6.0518e6),
  ...
)
*/
