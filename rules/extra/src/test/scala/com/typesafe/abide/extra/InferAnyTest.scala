package com.typesafe.abide.extra

import scala.tools.abide.traversal._
import com.typesafe.abide.extra._

class InferAnyTest extends TraversalTest {

  val rule = new InferAny(context)

  "Inferences of Any" should "not be valid if not annotated" ignore {
    val tree = fromString("""
      class Test {
        List(1, 2, 3) contains "a"
        1L to 10L contains 3
      }
    """)

    global.ask { () => apply(rule)(tree).size should be(2) }
  }

  ignore should "not be valid in methods if not annotated" in {
    val tree = fromString("""
      class Test {
        def get(x: => Option[Int]) = x getOrElse Some(5)
      }
    """)

    println(scala.reflect.runtime.universe.showRaw(tree))

    global.ask { () => apply(rule)(tree).size should be(0) }
  }

  ignore should "be valid if annotated with Any" in {
    val tree = fromString("""
      class Test {
        List(1, 2, 3) contains ("a": Any)
        1L to 10L contains (3: Any)
      }
    """)

    global.ask { () => apply(rule)(tree).isEmpty should be(true) }
  }

  ignore should "not be valid in methods if annotated" in {
    val tree = fromString("""
      class Test {
        def get(x: => Option[Int]) = x getOrElse (Some(5): Any)
      }
    """)

    global.ask { () => apply(rule)(tree).size should be(1) }
  }
}
