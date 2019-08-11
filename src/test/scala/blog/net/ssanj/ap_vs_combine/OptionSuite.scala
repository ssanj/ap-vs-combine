package blog.net.ssanj.ap_vs_combine

import boon._
import cats.implicits._

object OptionSuite extends SuiteLike("Option Suite") {

  //similar to x.getOrElse(y) when there are nones, but combines both if present, as `A` is a Monoid
  private val t1 = test("combine") {
    (some(10) combine some(20)) =?= Some(30) | "combine Some and Some" and
    (some(10) combine none[Int]) =?= Some(10) | "combine Some and None" and
    (none[Int] combine some(20)) =?= Some(20) | "combine None and Some" and
    (none[Int] combine none[Int]) =?= none[Int] | "combine None and Some" and
    (some(10) combine some(20) combine some(30)) =?= Some(60) | "combine Some and Some and Some" and
    (none[Int] combine some(20) combine some(30)) =?= Some(50) | "combine None and Some and Some" and
    (some(10) combine none[Int] combine some(30)) =?= Some(40) | "combine Some and None and Some" and
    (some(10) combine some(20) combine none[Int]) =?= Some(30) | "combine Some and None and None"
  }

  //similar to x.getOrElse(y)
  private val t2 = test("combineK") {
    (some(10) combineK some(20)) =?= Some(10) | "combineK Some and Some" and
    (some(10) combineK none[Int]) =?= Some(10) | "combineK Some and None" and
    (none[Int] combineK some(20)) =?= Some(20) | "combineK None and Some" and
    (none[Int] combineK none[Int]) =?= none[Int] | "combineK None and Some"
  }

  //product is similar to flatMap, in that any none leads to the full expression being none
  private val t3 = test("productL") {
    (some(10) productL some(20)) =?= Some(10) | "productL Some and Some" and
    (some(10) productL none[Int]) =?= none[Int] | "productL Some and None" and
    (none[Int] productL some(20)) =?= none[Int] | "productL None and Some" and
    (none[Int] productL none[Int]) =?= none[Int] | "productL None and Some"
  }

  private val t4 = test("productR") {
    (some(10) productR some(20)) =?= Some(20) | "productR Some and Some" and
    (some(10) productR none[Int]) =?= none[Int] | "productR Some and None" and
    (none[Int] productR some(20)) =?= none[Int] | "productR None and Some" and
    (none[Int] productR none[Int]) =?= none[Int] | "productR None and Some"
  }

  override val tests = oneOrMore(t1, t2, t3, t4)

  private def some[A](value: A): Option[A] = Some(value)
  
  private def none[A]: Option[A] = None  
}