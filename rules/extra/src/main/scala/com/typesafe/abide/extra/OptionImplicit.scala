package com.typesafe.abide.extra

import scala.tools.abide._
import scala.tools.abide.traversal._

class OptionImplicit(val context: Context) extends ScopingRule {
  import context.universe._

  type Owner = Boolean

  def inPattern = state.scope.headOption.nonEmpty

  val name = "option-implicit"

  case class Warning(val pos: Position, view: ApplyImplicitView) extends RuleWarning {
    val message = s"Suspicious application of an implicit view (${view.fun}) in the argument to Option.apply."
  }

  val step = optimize {
    case CaseDef(_, _, _) => enter(true)

    case app @ Apply(tap @ TypeApply(q"scala.Option.apply", targs), List(view: ApplyImplicitView)) if !inPattern =>
      nok(Warning(app.pos, view))
  }
}
