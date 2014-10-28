package com.typesafe.abide.extra

import scala.tools.abide._
import scala.tools.abide.traversal._

class NullaryUnit(val context: Context) extends WarningRule {
  import context.universe._

  val name = "nullary-unit"

  case class Warning(val pos: Position, name: Name) extends RuleWarning {
    val message = s"Side-effecting nullary methods are discouraged: try defining as `def $name()` instead"
  }

  val step = optimize {
    case defDef @ q"$mods def $name[..$tparams]: scala.Unit = $expr" =>
      nok(Warning(defDef.pos, name))
    case defDef @ q"$mods def $name: scala.Unit = $expr" =>
      nok(Warning(defDef.pos, name))
    case defDef @ q"$mods def $name[..$tparams] = $expr" if expr.tpe =:= typeOf[Unit] =>
      nok(Warning(defDef.pos, name))
    case defDef @ q"$mods def $name = $expr" if expr.tpe =:= typeOf[Unit] =>
      nok(Warning(defDef.pos, name))
  }
}
