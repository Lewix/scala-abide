package com.typesafe.abide.extra

import scala.tools.abide._
import scala.tools.abide.traversal._

class InferAny(val context: Context) extends PathRule {
  import context.universe._

  val name = "infer-any"

  type Element = Unit

  case class Warning(app: Tree, tpt: Tree) extends RuleWarning {
    val pos = app.pos
    val message = s"A type was inferred to be `${tpt.tpe.typeSymbol.name}`. This may indicate a programming error."
  }

  def containsAny(t: Type) =
    t.contains(typeOf[Any].typeSymbol) || t.contains(typeOf[AnyVal].typeSymbol)

  def isInferredAny(tree: Tree) = tree match {
    case tpt @ TypeTree() =>
      tpt.original == null &&
        (tpt.tpe.contains(typeOf[Any].typeSymbol) ||
          tpt.tpe.contains(typeOf[AnyVal].typeSymbol))
    case _ => false
  }

  def inSyntheticMethod = state.last.nonEmpty

  val step = optimize {
    case df @ DefDef(_, _, _, _, _, _) if df.symbol.isSynthetic => enter(())

    case app @ Apply(TypeApply(fun, targs), args) if targs.exists(isInferredAny) && !inSyntheticMethod =>
      val existsExplicitAny = args.map(_.tpe).exists { t =>
        (t.contains(typeOf[Any].typeSymbol) || t.contains(typeOf[AnyVal].typeSymbol))
      }
      if (!existsExplicitAny) {
        nok(Warning(app, targs.find(isInferredAny).get))
      }
  }

}
