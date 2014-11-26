package com.typesafe.abide.extra

import scala.tools.abide.traversal._
import com.typesafe.abide.extra._

class MissingInterpolatorTest extends TraversalTest {

  val rule = new MissingInterpolator(context)

  "String literal" should "not be valid if it contains quoted names of values in scope" in {
    val tree = fromString("""
      class A {
        val bippy = 123

        def f = "Put the $bippy in the $bippy" // warn
      }
    """)

    global.ask { () => apply(rule)(tree).size should be(1) }
  }

  it should "be valid if it contains quoted names of values not in scope" in {
    val tree = fromString("""
      class B {
        val dingus = 123

        def f = "Put the $bippy in the $bippy" // no warn
      }
    """)

    global.ask { () => apply(rule)(tree).size should be(0) }
  }

  it should "not be valid if it contains quoted code" in {
    val delim = "\"\"\""
    val tree = fromString("""
      class C {
        def f = """ + delim + """Put the ${println("bippy")} in the bippy!""" + delim + """
      }
    """)

    global.ask { () => apply(rule)(tree).size should be(1) }
  }

  it should "not be valid if it contains quoted names of methods defined in the same package" in {
    val tree = fromString("""
      package object test {
        def aleppo = 9
      }
      package test {
        class E {
          def f = "$aleppo is a pepper and a city." // warn
          def k = s"Just an interpolation of $aleppo" // no warn
        }
      }
    """)

    global.ask { () => apply(rule)(tree).size should be(1) }
  }

  it should "not be valid if it contains quoted names of private methods defined in parent class" in {
    val tree = fromString("""
      class Bar {
        private def bar = 8
        if (bar > 8) ???       // use it to avoid extra warning
      }
      class Baz extends Bar {
        def f = "$bar is private, shall we warn just in case?" // warn
      }
    """)

    global.ask { () => apply(rule)(tree).size should be(1) }
  }

  it should "not be valid if it contains quoted names of methods which take no explicit arguments" in {
    val tree = fromString("""
      package object test {
        def greppo(n: Int) = ???
        def zappos(n: Int)(implicit ord: math.Ordering[Int]) = ???
        def hippo(implicit n: Int) = ???
      }
      package test {
        class G {
          def g = "$greppo takes an arg" // no warn
          def z = "$zappos takes an arg too" // no warn
          def h = "$hippo takes an implicit" // warn
        }
      }
    """)

    global.ask { () => apply(rule)(tree).size should be(1) }
  }

  it should "be valid if it contains quoted names of methods which take explicit arguments" in {
    val tree = fromString("""
      class J {
        def j = 8
        class J2 {
          def j(i: Int) = 2 * i
          def jj = "shadowed $j"  // no warn
        }
      }
    """)

    global.ask { () => apply(rule)(tree).size should be(0) }
  }

  it should "be valid in annotations if it contains quoted names of objects not in scope" in {
    val tree = fromString("""
      package test {
        @scala.annotation.implicitNotFound(msg = "Cannot construct a collection of type ${To} with elements of type ${Elem} based on a collection of type ${From}.") // no warn
        trait CannotBuildFrom1[-From, -Elem, +To]

        @scala.annotation.implicitNotFound("Cannot construct a collection of type ${To} with elements of type ${Elem} based on a collection of type ${From}.") // no warn
        trait CannotBuildFrom2[-From, -Elem, +To]

        import annotation._
        @implicitNotFound(msg = "Cannot construct a collection of type ${To} with elements of type ${Elem} based on a collection of type ${From}.") // no warn
        trait CannotBuildFrom3[-From, -Elem, +To]

        @implicitNotFound("No Z in ${A}")   // no warn
        class Z[A]
      }
    """)

    global.ask { () => apply(rule)(tree).size should be(0) }
  }

  it should "not be valid if it contains quoted names of methods with one overloaded alternatives which take no explicit arguments" in {
    val tree = fromString("""
        class Doo {
          def beppo(i: Int) = 8 * i
          def beppo = 8
          class Dah extends Doo {
            def f = "$beppo was a marx bros who saw dollars."  // warn
          }
        }
      }
    """)

    global.ask { () => apply(rule)(tree).size should be(1) }
  }

  it should "be valid if it contains quoted names of methods which take curried explicit arguments" in {
    val tree = fromString("""
      package curry1 {
        def bunko()(x: Int): Int = 5
        def f1 = "I was picked up by the $bunko squad" // no warn
      }
    """)

    global.ask { () => apply(rule)(tree).size should be(0) }
  }

  it should "not be valid if it contains quoted names of unitary methods" in {
    val tree = fromString("""
      package curry2 {
        def groucho(): Int = 5
        def f2 = "I salute $groucho" // warn
      }
    """)

    global.ask { () => apply(rule)(tree).size should be(1) }
  }

  it should "not be valid if it contains quoted names of methods with many empty argument lists" in {
    val tree = fromString("""
      package curry3 {
        def dingo()()()()()(): Int = 5 // kind of nuts this can be evaluated with just 'dingo', but okay
        def f3 = "I even salute $dingo" // warn
      }
    """)

    global.ask { () => apply(rule)(tree).size should be(1) }
  }

  it should "not be valid if it contains quoted names of methods with many empty argument lists and type arguments" in {
    val tree = fromString("""
      package curry4 {
        def calico[T1, T2]()()(): Int = 5 // even nutsier
        def f4 = "I also salute $calico" // warn 9
      }
    """)

    global.ask { () => apply(rule)(tree).size should be(1) }
  }

  it should "be valid if it contains quoted names of methods with at least one non-empty argument list" in {
    val tree = fromString("""
      package curry5 {
        def palomino[T1, T2]()(y: Int = 5)(): Int = 5 // even nutsier
        def f5 = "I draw the line at $palomino" // no warn
      }
    """)

    global.ask { () => apply(rule)(tree).size should be(0) }
  }
}

//TODO: perverse macro test
//package t8013
//
//// perverse macro to confuse Xlint
//
//import scala.language.experimental.macros
//import scala.reflect.macros.{ BlackboxContext => Context }
//
//object Perverse {
//
//  implicit class Impervolator(sc: StringContext) {
//    def p(args: Any*): String = macro pImpl
//  }
//
//  // turn a nice interpolation into something that looks
//  // nothing like an interpolation or anything we might
//  // recognize, but which includes a "$id" in an apply.
//  def pImpl(c: Context)(args: c.Expr[Any]*): c.Expr[String] = {
//    import c.universe._
//    val macroPos = c.macroApplication.pos
//    val text = macroPos.lineContent substring macroPos.column
//    val tt = Literal(Constant(text))
//    val tree = q"t8013.Perverse.pervert($tt)"
//    c.Expr[String](tree)
//  }
//
//  // identity doesn't seem very perverse in this context
//  //def pervert(text: String): String = text
//  def pervert(text: String): String = {
//    Console println s"Perverting [$text]"
//    text
//  }
//}
//
///*
// * scalac: -Xfatal-warnings -Xlint
// */
//package t8013
//
//// unsuspecting user of perverse macro
//trait User {
//  import Perverse.Impervolator
//  val foo = "bar"
//  Console println p"Hello, $foo"
//}
