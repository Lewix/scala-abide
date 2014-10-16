package com.typesafe.abide.extra

import scala.tools.abide._
import scala.tools.abide.traversal._

class InferAny(val context: Context) extends WarningRule {
  import context.universe._

  val name = "infer-any"

  case class Warning() extends RuleWarning {
    val pos = ???
    val message = ???
  }

  val step = optimize {
    case app @ Apply(fn, args) if fn.symbol.isMethod =>
      val tparams = fn.symbol.asMethod.typeParams
      val argTypes = args.map(_.tpe)

      val methodSym = fn.symbol.asMethod
      println(s"Method ${fn.symbol.name} type arguments: ${methodSym.typeParams}")
      println(s"Free types ${app.freeTypes}")
      println(s"fn.tpe is ${fn.tpe}")
      println(s"fn.tpe.typeSymbol.typeParams is ${fn.tpe.typeSymbol.typeParams}")
      println(s"fn.tpe.typeSymbol.Is specialized ${fn.tpe.typeSymbol.isSpecialized}")
      //println(s"fn.tpe.typeSymbol.toTypeConstructor ${fn.tpe.typeSymbol.toTypeConstructor}")
      println(s"args.map(_.tpe) is ${args.map(_.tpe)}")

      for (t <- args.map(_.tpe)) {
        val typeSymbol = t.typeSymbol.asType
        println("Type symbol info")
        println(s"Is abstract type ${typeSymbol.isAbstractType}")
        println(s"Is specialized ${typeSymbol.isSpecialized}")
        println(s"Is parameter ${typeSymbol.isParameter}")
        //println(s"Type signature ${typeSymbol.typeSignature}")
        println(s"Is skolem ${typeSymbol.isSkolem}")
        println(s"Type constr. ${typeSymbol.toTypeConstructor}")
      }
  }

}
