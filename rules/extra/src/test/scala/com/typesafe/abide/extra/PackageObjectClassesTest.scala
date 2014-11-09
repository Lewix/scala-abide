package com.typesafe.abide.extra

import scala.tools.abide.traversal._
import com.typesafe.abide.extra._

class PackageObjectClassesTest extends TraversalTest {

  val rule = new PackageObjectClasses(context)

  "Class definitions" should "not be valid if in a package object" in {
    val tree = fromString("""
      package object test {
        class A
        case class B()
        private class C { def x: Int = 3 }
      }
    """)

    global.ask { () => apply(rule)(tree).size should be(3) }
  }

  it should "be valid if not in a package object" in {
    val tree = fromString("""
      package test2 {
        class A
        case class B()
        private class C { def x: Int = 3 }
      }
    """)

    global.ask { () => apply(rule)(tree).isEmpty should be(true) }
  }
}
