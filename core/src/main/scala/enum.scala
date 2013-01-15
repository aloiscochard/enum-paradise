package scalax

object Days extends Enum(Monday, Tuesday, Wednesday, Thursday, Friday)

object DaysDemo {
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
}

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

object MonthsDemo {
  def code = Months.values.map(_.name.toUpperCase)

  //scala> scalax.MonthsDemo.code
  //res1: Seq[String] = List(JAN, FEB, MAR, APR, MAY, JUN, JUL, AUG, SEP, OCT, NOV, DEC)
}
