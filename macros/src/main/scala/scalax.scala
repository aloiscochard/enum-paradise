import language.experimental.macros
import scala.reflect.macros.Context

package object scalax {
  trait Enumerable { 
    type Value <: scalax.Value
    def values: Seq[Value]
  }

  trait Value {
    def name: String
    override def toString: String = name
  }

  type Enum(symbol: Symbol*) = macro Macros.enum

  object Macros {
    def enum(c: Context)(symbol: c.Expr[Symbol]*): c.Tree = {
      import c.universe._
      import Flag._

      val names = symbol.map { case Expr(Apply((_, Literal(Constant(x: String)) :: Nil))) => x }

      val valueObjects = names.toList.map { name =>
        ModuleDef(
          Modifiers(),
          TermName(name),
          Template(
            List(Select(This(TypeName(c.enclosingImpl.name.toString)), TypeName("Val"))),
            emptyValDef,
            List(
              DefDef(
                Modifiers(), 
                nme.CONSTRUCTOR, 
                List(), 
                List(List()), 
                TypeTree(), 
                Block(List(Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), List())), Literal(Constant(())))
              ), 
              DefDef(Modifiers(), TermName("name"), List(), List(), TypeTree(), Literal(Constant(name)))
            )
          )
        )
      }

      val valuesList = ValDef(
        Modifiers(),
        TermName("values"),
        AppliedTypeTree(Ident(TypeName("Seq")), List(Ident(TypeName("Value")))),
        Apply(
          Select(
            Select(Select(Select(Ident(TermName("scala")), TermName("collection")), TermName("immutable")), TermName("List")),
            TermName("apply")
          ), 
          names.map(name => Ident(TermName(name))).toList
        )
      )


      val Expr(Block(List(valueSealedTrait), _)) = reify {
        sealed trait Val extends scalax.Value
      }

      val valueType = TypeDef(Modifiers(OVERRIDE), TypeName("Value"), List(), Ident(TypeName("Val")))

      val generatedCode = valueSealedTrait :: valueType :: valuesList :: valueObjects

      val Expr(Block(List(ClassDef(_, _, _, Template(parents, self, body))), _)) = reify {
        class CONTAINER extends Enumerable
      }

      /*
      println(showRaw(reify {
        class CONTAINER extends Enumerable {
          sealed trait Value extends scalax.Value
          override type T = Value
        }
      }))
      */

     
      // TODO Support existing code (need to remove constructor of body for that...)
      val Template(_, _, existingCode) = c.enclosingTemplate
      Template(parents, emptyValDef, /*existingCode ++*/ body ++ generatedCode)
    }

    //type Enum[T <: Value](symbol: Symbol*) = macro enum[T]
  }
}
