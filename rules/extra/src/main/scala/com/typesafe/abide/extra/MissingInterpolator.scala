package com.typesafe.abide.extra

import scala.tools.abide._
import scala.tools.abide.traversal._

class MissingInterpolator(val context: Context) extends WarningRule {
  import context.universe._

  val name = "missing-interpolator"

  case class Warning() extends RuleWarning {
    val pos: Position = ???
    val message: String = ???
  }

  val step = optimize {
    case q"val frefer = 43434" => ()
  }
}
