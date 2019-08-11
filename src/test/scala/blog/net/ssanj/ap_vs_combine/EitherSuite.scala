package blog.net.ssanj.ap_vs_combine

import boon._
import cats.implicits._

object EitherSuite extends SuiteLike("Either Suite") {

  private   val combineTable = truthTable(
    (right(10), right(20)) -> tval(right(30)),
    (right(10), left[Int]("error1")) -> tval(left[Int]("error1")),
    (left[Int]("error1"), right(20)) -> tval(left[Int]("error1")),
    (left[Int]("error1"), left[Int]("error2")) -> tval(left[Int]("error1"))
  )

  private val combineTest = table("combine", combineTable)(t => t._1 combine t._2)  

  private val combineExtendTable = truthTable(
    (right(10), right(20), right(30)) -> tval(right(60)),
    (left[Int]("error1"), right(20), right(30)) -> tval(left[Int]("error1")),
    (right(10), left[Int]("error1"), right(30)) -> tval(left[Int]("error1")),
    (right(10), right(20), left[Int]("error1")) -> tval(left[Int]("error1")),
    (left[Int]("error1"), left[Int]("error2"), left[Int]("error3")) -> tval(left[Int]("error1"))
  )

  private val combineExtendedTest = table("combineExtended", combineExtendTable)(t => t._1 combine t._2 combine t._3)

  private val combineKTable = truthTable(
    (right(10), right(20)) -> tval(right(10)),
    (right(10), left[Int]("error1")) -> tval(right(10)),
    (left[Int]("error1"), right(20)) -> tval(right(20)),
    (left[Int]("error1"), left[Int]("error2")) -> tval(left[Int]("error2"))
  )

  //similar to x.getOrElse(y)
  private val combineKTest = table("combineK", combineKTable)(t => t._1 combineK t._2)


  private val productLTable = truthTable(
    (right(10), right(20)) -> tval(right(10)),
    (right(10), left[Int]("error1")) -> tval(left[Int]("error1")),
    (left[Int]("error1"), right(20)) -> tval(left[Int]("error1")),
    (left[Int]("error1"), left[Int]("error2")) -> tval(left[Int]("error1"))
  )

  //product is similar to flatMap
  private val productLTest = table("productL", productLTable)(t => t._1 productL t._2)

  private val productRTable = truthTable(
    (right(10), right(20)) -> tval(right(20)),
    (right(10), left[Int]("error1")) -> tval(left[Int]("error1")),
    (left[Int]("error1"), right(20)) -> tval(left[Int]("error1")),
    (left[Int]("error1"), left[Int]("error2")) -> tval(left[Int]("error1"))
  )

  private val productRTest = table("productR", productRTable)(t => t._1 productR t._2)

  override val tests = oneOrMore(combineTest, combineExtendedTest, combineKTest ,productLTest, productRTest)

  private def right[A](value: A): Either[String, A] = Right(value)
  
  private def left[A](error: String): Either[String, A] = Left(error)  
}