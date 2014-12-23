package com.typesafe.abide.extra

import scala.tools.abide._
import scala.tools.abide.traversal._

class TypeParameterShadow(val context: Context) extends ScopingRule {
  import context.universe._

  val name = "type-parameter-shadow"

  case class Warning(tp: TypeDef, sym1: Symbol, sym2: Symbol) extends RuleWarning {
    val pos: Position = tp.pos
    val message: String = s"Type parameter ${tp.name} defined in $sym1 shadows $sym2 defined in ${sym2.owner}. You may want to rename your type parameter, or possibly remove it."
  }

  // State is a pair of List[Symbol] (the declared type parameters) and Boolean
  // (true if the declarer is a class or method or type member)
  type Owner = Symbol

  def getDeclaration(tp: TypeDef, scope: List[Owner]) = scope.find { sym =>
    sym.name == tp.name && sym.isType
  }

  def enclClassOrMethodOrTypeMemberScope: List[Owner] = state.scope.dropWhile { sym =>
    !sym.isClass && !sym.isMethod && !(sym.isType && !sym.isParameter)
  }

  def warnTypeParameterShadow(tparams: List[TypeDef], sym: Symbol) = {
    if (!sym.isSynthetic) {
      val tt = tparams.filter(_.name != typeNames.WILDCARD).foreach { tp =>
        // We don't care about type params shadowing other type params in the same declaration
        getDeclaration(tp, enclClassOrMethodOrTypeMemberScope) foreach { prevTp =>
          if (prevTp != tp.symbol) {
            println("Warn")
            nok(Warning(tp, sym, prevTp))
          }
        }
      }
    }
  }

  def enterAll(possTypes: List[Symbol]) = possTypes.map(enter(_))

  val step = optimize {
    case t @ Template(parents, self, body) =>
      enterAll(t.tpe.members.toList)
    case p @ PackageDef(pid, stats) =>
      enterAll(pid.tpe.members.toList)

    case cd @ ClassDef(_, _, tparams, _) =>
      enter(cd.symbol)
      warnTypeParameterShadow(tparams, cd.symbol)
      enterAll(tparams.map(_.symbol))
    case dd @ DefDef(_, _, tparams, _, _, _) =>
      enter(dd.symbol)
      warnTypeParameterShadow(tparams, dd.symbol)
      enterAll(tparams.map(_.symbol))
    case td @ TypeDef(_, _, tparams, _) =>
      enter(td.symbol)
      warnTypeParameterShadow(tparams, td.symbol)
      enterAll(tparams.map(_.symbol))
  }
}

//TODO: test package object with methods shadowed by tparams
//TODO: test shadowed type aliases