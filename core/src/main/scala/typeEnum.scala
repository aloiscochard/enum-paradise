package scalax

trait Serializable[T] { def serialize(x: T): String }

object Serializable extends TypeEnum[Serializable](
  Int { def serialize(x: Int) = x.toString },
  Double { def serialize(x: Double) = x.toString }
)

object SerializableDemo {
  def serialize[T : Serializable](x: T) = implicitly[Serializable[T]].serialize(x)

  serialize(42)
  serialize(math.Pi)
}
