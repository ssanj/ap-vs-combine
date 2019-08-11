package blog.net.ssanj.ap_vs_combine

import boon._
import cats.implicits._

object OptionSuite extends SuiteLike("Option Suite") {

  private val combineTable = truthTable(
    (some(10), some(20)) -> tval(some(30)),
    (some(10), none[Int]) -> tval(some(10)),
    (none[Int], some(20)) -> tval(some(20)),
    (none[Int], none[Int]) -> tval(none[Int])
  )

  //similar to x.getOrElse(y) when there are nones, but combines both if present, as `A` is a Monoid
  private val combineTest = table("combine", combineTable)(t => t._1 combine t._2)

  private val combineExtendedTable = truthTable(
    (some(10), some(20), some(30)) -> tval(some(60)),
    (none[Int], some(20), some(30)) -> tval(some(50)),
    (some(10), none[Int], some(30)) -> tval(some(40)),
    (some(10), some(20), none[Int]) -> tval(some(30))
  )

  private val combineExtendedTest = table("combineExtended", combineExtendedTable)(t => t._1 combine t._2 combine t._3)

  private val combineKTable = truthTable(
    (some(10), some(20)) -> tval(some(10)),
    (some(10), none[Int]) -> tval(some(10)),
    (none[Int], some(20)) -> tval(some(20)),
    (none[Int], none[Int]) -> tval(none[Int])
  )

  //similar to x.getOrElse(y
  private val combineKTest = table("combineK", combineKTable)(t => t._1 combineK t._2)

  private val productLTable = truthTable(
    (some(10), some(20)) -> tval(Some(10)),    
    (some(10), none[Int]) -> tval(none[Int]),  
    (none[Int], some(20)) -> tval(none[Int]),  
    (none[Int], none[Int]) -> tval(none[Int])
  )

  //product is similar to flatMap, in that any none leads to the full expression being none
  private val productLTest = table("productL", productLTable)(t => t._1 productL t._2)

  private val productRTable = truthTable(
    (some(10), some(20)) -> tval(some(20)),
    (some(10), none[Int]) -> tval(none[Int]),
    (none[Int], some(20)) -> tval(none[Int]),
    (none[Int], none[Int]) -> tval(none[Int])
  )

  private val productRTest = table("productR", productRTable)(t => t._1 productR t._2)

  override val tests = oneOrMore(combineTest, combineExtendedTest, combineKTest, productLTest, productRTest)

  private def some[A](value: A): Option[A] = Some(value)
  
  private def none[A]: Option[A] = None  
}