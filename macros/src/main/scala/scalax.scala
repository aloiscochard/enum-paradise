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

  type Enum(values: _*) = macro Macros.enum
  type EnumOf[T <: Value](values: _*) = macro Macros.enumOf[T]

  case class EnumDef(id: String, name: String)

  object Macros {
    // TODO Factorize common macros code

    def enum(c: Context)(values: c.Tree*): c.Tree = {
      import c.universe._
      import Flag._

      val enumDefs = values.toList.collect {
        case Ident(TermName(id)) => EnumDef(id, id)
        case Apply(Ident(TermName(id)), List(Literal(Constant(name)))) => EnumDef(id, name.toString)
      }

      val valueObjects = enumDefs.map { enumDef =>
        ModuleDef(
          Modifiers(),
          TermName(enumDef.id),
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
              DefDef(Modifiers(), TermName("name"), List(), List(), TypeTree(), Literal(Constant(enumDef.name)))
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
          enumDefs.map(enumDef => Ident(TermName(enumDef.id))).toList
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
     
      val Template(_, _, _ :: existingCode) = c.enclosingTemplate
      Template(parents, emptyValDef, body ++ generatedCode ++ existingCode)
    }


    def enumOf[T : c.WeakTypeTag](c: Context)(values: c.Tree*): c.Tree = {
      import c.universe._
      import Flag._

      implicit val context = c

      val tpe = c.weakTypeOf[T]
      val names = values.map(_.toString)

      val valueObjects = names.toList.map { name =>
        ModuleDef(
          Modifiers(),
          TermName(name),
          Template(
            List(Ident(tpe.typeSymbol)),
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
        AppliedTypeTree(Ident(TypeName("Seq")), List(TypeTree(tpe))),
        Apply(
          Select(
            Select(Select(Select(Ident(TermName("scala")), TermName("collection")), TermName("immutable")), TermName("List")),
            TermName("apply")
          ), 
          names.map(name => Ident(TermName(name))).toList
        )
      )

      val valueType = TypeDef(Modifiers(OVERRIDE), TypeName("Value"), List(), Ident(tpe.typeSymbol))

      val generatedCode = valueType :: valuesList :: valueObjects

      val Expr(Block(List(ClassDef(_, _, _, Template(parents, self, body))), _)) = reify {
        class CONTAINER extends Enumerable
      }
     
      val Template(_, _, _ :: existingCode) = c.enclosingTemplate
      Template(parents, emptyValDef, body ++ generatedCode ++ existingCode)
    }

  }
}
