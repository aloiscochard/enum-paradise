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

    def enum(c: Context)(values: c.Tree*): c.Tree = {
      import c.universe._
      import Flag._

      val enumDefs = parseValues(c)(values.toList)

      val Expr(Block(List(valueSealedTrait), _)) = reify {
        sealed trait Val extends scalax.Value
      }

      val valueType = TypeDef(Modifiers(OVERRIDE), TypeName("Value"), List(), Ident(TypeName("Val")))
      val valueTypeTree = Select(This(TypeName(c.enclosingImpl.name.toString)), TypeName("Val"))

      template(c)(valueSealedTrait :: valueType :: valuesList(c)(valueTypeTree, enumDefs) :: valueObjects(c)(valueTypeTree, enumDefs))
    }


    def enumOf[T : c.WeakTypeTag](c: Context)(values: c.Tree*): c.Tree = {
      import c.universe._
      import Flag._

      implicit val context = c

      val tpe = c.weakTypeOf[T]
      val enumDefs = parseValues(c)(values.toList)

      val valueTypeTree = Ident(tpe.typeSymbol)
      val valueType = TypeDef(Modifiers(OVERRIDE), TypeName("Value"), List(), Ident(tpe.typeSymbol))

      val generatedCode = valueType ::
        valuesList(c)(valueTypeTree, enumDefs) ::
        valueObjects(c)(valueTypeTree, enumDefs)

      template(c)(valueType :: valuesList(c)(valueTypeTree, enumDefs) :: valueObjects(c)(valueTypeTree, enumDefs))
    }

    private def parseValues(c: Context)(xs: List[c.Tree]): List[EnumDef] = {
      import c.universe._
      xs.collect {
        case Ident(TermName(id)) => EnumDef(id, id)
        case Apply(Ident(TermName(id)), List(Literal(Constant(name)))) => EnumDef(id, name.toString)
      }
    }

    private def template(c: Context)(generatedCode: List[c.Tree]) = {
      import c.universe._

      val Expr(Block(List(ClassDef(_, _, _, Template(parents, self, body))), _)) = reify {
        class CONTAINER extends Enumerable
      }
     
      val Template(_, _, _ :: existingCode) = c.enclosingTemplate
      Template(parents, emptyValDef, body ++ generatedCode ++ existingCode)
    }

    private def valueObjects(c: Context)(typeTree: c.Tree, enumDefs: List[EnumDef]): List[c.Tree] = enumDefs.map { enumDef =>
      import c.universe._
      ModuleDef(
        Modifiers(),
        TermName(enumDef.id),
        Template(
          List(typeTree),
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

    private def valuesList(c: Context)(typeTree: c.Tree, enumDefs: List[EnumDef]): c.Tree = {
      import c.universe._
      ValDef(
        Modifiers(),
        TermName("values"),
        AppliedTypeTree(Ident(TypeName("Seq")), List(typeTree)),
        Apply(
          Select(
            Select(Select(Select(Ident(TermName("scala")), TermName("collection")), TermName("immutable")), TermName("List")),
            TermName("apply")
          ), 
          enumDefs.map(enumDef => Ident(TermName(enumDef.id))).toList
        )
      )
    }
  }
}
