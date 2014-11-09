package com.typesafe.abide.extra

import scala.tools.abide._
import scala.tools.abide.traversal._

class UnsoundMatch(val context: Context) extends ScopingRule {
  import context.universe._

  abstract class Location { val pt: Type }
  case class InCaseDef(pt: Type) extends Location
  case class OutsideCaseDef(pt: Type) extends Location

  type Owner = Location

  val name = "unsound-match"

  case class Warning(pat: Tree, pt: Type, binder: Name) extends RuleWarning {
    val pos = pat.pos
    val message =
      s"""The value matched by $pat is bound to $binder, which may be used under the
          |unsound assumption that it has type ${pat.tpe}, whereas we can only safely
          |count on it having type $pt, as the pattern is matched using `==` (see SI-1503).""".stripMargin
  }

  def isMatchedUsingEquals(caseDef: Tree) = {
    caseDef match {
      case Select(_, _)          => true
      case Bind(_, Select(_, _)) => true
      case Ident(_)              => true
      case Bind(_, Ident(_))     => true
      case _                     => false
    }
  }

  def isInCaseDef: Boolean = state.scope.headOption match {
    case Some(InCaseDef(_)) => true
    case _                  => false
  }

  def safeType: Type = state.scope.head.pt

  val step = optimize {
    case q"$expr match { case ..$cases }" =>
      enter(OutsideCaseDef(expr.tpe))

    case CaseDef(_, _, _) =>
      enter(InCaseDef(safeType))

    case b @ Bind(binder, pat) if binder != nme.WILDCARD && isInCaseDef && !(safeType <:< pat.tpe) && isMatchedUsingEquals(b) =>
      nok(Warning(pat, state.scope.head.pt, binder))
  }
}
