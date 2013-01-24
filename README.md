# Enum Paradise (WORK IN PROGRESS)

Scala enumeration implementation using type macros provided by [Macro Paradise](http://docs.scala-lang.org/overviews/macros/paradise.html)

## Usage

Enumeration:

    object Days extends Enum(Monday, Tuesday, Wednesday, Thursday, Friday)

    implicitly[Days.Monday.type <:< Days.Value]

    import Days._
    def f(x: Value) = x match {
      case Monday => 
      case Tuesday => 
      case Wednesday => 
      case Thursday => 
      //case Friday => 
    }
    //[warn] /core/src/main/scala/enum.scala:10: match may not be exhaustive.
    //[warn] It would fail on the following input: Friday

Enumeration with specific names:

    object Months extends Enum(
      January("Jan"),
      February("Feb"),
      Mars("Mar"),
      April("Apr"),
      May,
      June("Jun"),
      July("Jul"),
      August("Aug"),
      September("Sep"),
      October("Oct"),
      November("Nov"),
      December("Dec")
    )

    def code = Months.values.map(_.name.toUpperCase)

    //scala> scalax.MonthsDemo.code
    //res1: Seq[String] = List(JAN, FEB, MAR, APR, MAY, JUN, JUL, AUG, SEP, OCT, NOV, DEC)

Enumeration with specific value type:

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

    implicitly[Planets.Value =:= Planet]

    //scala> Planets.values.sortBy(_.mass)
    //res0: Seq[scalax.Planet] = List(Mercury, Mars, Venus, Earth, Uranus, Neptune, Saturn, Jupiter)

## License

    This software is licensed under the Apache 2 license, quoted below.

    Copyright 2009-2012 Alois Cochard 

    Licensed under the Apache License, Version 2.0 (the "License"); you may not
    use this file except in compliance with the License. You may obtain a copy of
    the License at http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
    WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
    License for the specific language governing permissions and limitations under
    the License.
