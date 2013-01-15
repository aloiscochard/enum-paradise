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

    sealed trait Planet extends Value

    object Planets extends EnumOf[Planet](
      Mercury,
      Venus
    )

    implicitly[Planets.Value =:= Planet]

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
