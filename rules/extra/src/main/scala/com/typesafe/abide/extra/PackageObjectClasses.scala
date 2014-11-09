package com.typesafe.abide.extra

import scala.tools.abide._
import scala.tools.abide.traversal._

class PackageObjectClasses(val context: Context) extends WarningRule {
  import context.universe._

  val name = "package-object-classes"

  case class Warning(val pos: Position, symbol: Symbol) extends RuleWarning {
    val message =
      s"""It is not recommended to define classes/objects inside of package objects.
         |If possible, define ${symbol} in ${symbol.owner.owner} instead.""".stripMargin
  }

  def isPackageObjectClass(sym: Symbol) =
    sym.owner.isModuleClass && sym.owner.name == tpnme.PACKAGE

  val step = optimize {
    case cd @ ClassDef(_, _, _, _) if isPackageObjectClass(cd.symbol) =>
      nok(Warning(cd.pos, cd.symbol))
  }
}
