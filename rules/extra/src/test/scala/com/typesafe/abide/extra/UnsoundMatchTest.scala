package com.typesafe.abide.extra

import scala.tools.abide.traversal._
import com.typesafe.abide.extra._

class UnsoundMatchTest extends TraversalTest {

  val rule = new UnsoundMatch(context)

  "Matches with binders" should "not be valid when bound to an object" in {
    val tree = fromString("""
      class Test {
        case class L()
        object N extends L()

        L() match {
          case x@N => println(x) // cannot assume that x has type N.type
        }
      }
    """)

    global.ask { () => apply(rule)(tree).size should be(1) }
  }

  it should "be valid when bound to a class" in {
    val tree = fromString("""
      class Test {
        class L()
        case class N() extends L()

        new L() match {
          case x@N() => println(x)
          case _ => ()
        }
      }
    """)

    global.ask { () => apply(rule)(tree).isEmpty should be(true) }
  }

  def fun[T](x: Int): Int = 34

  it should "be valid when the binder is a wildcard" in {
    val tree = fromString("""
      class Test {
        case class L()
        object N extends L()

        L() match {
          case _ @ N => ()
        }
      }
    """)

    global.ask { () => apply(rule)(tree).isEmpty should be(true) }
  }

  it should "be valid when the expected type is the same as the binder's type" in {
    val tree = fromString("""
      class Test {
        case class L()
        object N extends L()

        N match {
          case x@N => println(x)
        }
      }
    """)

    global.ask { () => apply(rule)(tree).isEmpty should be(true) }
  }

  it should "not be valid for each binding with an unsound type" in {
    val tree = fromString("""
      class Test {
        class L()
        object N1 extends L()
        case class N2() extends L()
        case object N3 extends L()

        new L() match {
          case x@N1 => println(x)
          case x@N2() => println(x)
          case x@N3 => println(x)
          case x: L => println(x)
        }
      }
    """)

    global.ask { () => apply(rule)(tree).size should be(2) }
  }

  it should "not be valid for identifiers which reference objects" in {
    val tree = fromString("""
      class Test {
        class L()
        object N1 extends L()
        case class N2() extends L()

        def test() = {
          val n1 = N1
          val n2 = N2()
          val l = new L()

          new L() match {
            case x @ `n1` => println(x)
            case x @ `n2` => println(x)
            case x @ `l` => println(x)
          }
        }
      }
    """)

    global.ask { () => apply(rule)(tree).size should be(2) }
  }

  it should "not be valid for nested bindings which are invalid" in {
    val tree = fromString("""
      class Test {
        case class L()
        object N extends L()
        case class M(x: L)

        M(L()) match {
          case M(x @ N) => println(x)
        }
      }""")

    global.ask { () => apply(rule)(tree).size should be(1) }
  }

  it should "be valid for nexted bindings which are valid" in {
    val tree = fromString("""
      class Test {
        case class L()
        object N extends L()
        case class M(x: L)

        M(L()) match {
          case M(x @ L()) => println(x)
        }
      }""")

    global.ask { () => apply(rule)(tree).isEmpty should be(true) }
  }

  //TODO: partial functions generate a synthetic isDefinedAt method which gets
  //  traversed so there are two warnings here
  ignore should "not be valid for methods which use partial functions which are invalid" in {
    val tree = fromString("""
      class Test {
        case class L()
        object N extends L()

        List(L()) collect {
          case x@N => println(x) // cannot assume that x has type N.type
        }
      }""")

    global.ask { () => apply(rule)(tree).size should be(1) }
  }
}
