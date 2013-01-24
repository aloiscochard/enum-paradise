package scalax

sealed abstract class Planet(val mass: Double, val radius: Double) extends Value

object Planets extends EnumOf[Planet](
  Mercury (3.303e+23, 2.4397e6),
  Venus   (4.869e+24, 6.0518e6),
  Earth   (5.976e+24, 6.37814e6),
  Mars    (6.421e+23, 3.3972e6),
  Jupiter (1.9e+27,   7.1492e7),
  Saturn  (5.688e+26, 6.0268e7),
  Uranus  (8.686e+25, 2.5559e7),
  Neptune (1.024e+26, 2.4746e7)
) {
  def giants = List(Jupiter, Saturn)
}

object PlanetsDemo {
  import Planets._

  implicitly[Value =:= Planet]

  def f(x: Planet) = x match {
    case Mercury => 
    case Venus => 
  }

  Planets.values.sortBy(_.mass)
}
