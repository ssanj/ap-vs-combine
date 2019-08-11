package blog.net.ssanj.ap_vs_combine

import boon._
import cats.implicits._
import cats.data.Validated
import cats.data.NonEmptyList
import cats.data.ValidatedNel
import boon.model.StringRep
import boon.model.Difference

object ValidatedSuite extends SuiteLike("Validated Suite") {

  private[ValidatedSuite] object BoonCatsInstances {

    implicit def nonEmptyListStringRep[A: StringRep] = StringRep.from[NonEmptyList[A]](_.map(StringRep[A].strRep).toList.mkString("NEL(", ",", ")"))  

    implicit def validatedStringRep[A: StringRep, B: StringRep] =
      StringRep.from[Validated[A, B]](_.fold(l => s"Invalid(${StringRep[A].strRep(l)})", r => s"Valid(${StringRep[B].strRep(r)})"))

    implicit def nonEmptyListDifference[A: StringRep] = Difference.colDifference[A, NonEmptyList](_.toList)

    implicit def validatedDifference[A: StringRep, B: StringRep] = Difference.genericDifference[Validated[A, B]]
  }

  import BoonCatsInstances._

  private   val combineTable = truthTable(
    (valid(10), valid(20)) -> tval(valid(30)),
    (valid(10), invalid[Int]("error1")) -> tval(invalid[Int]("error1")),
    (invalid[Int]("error1"), valid(20)) -> tval(invalid[Int]("error1")),
    (invalid[Int]("error1"), invalid[Int]("error2")) -> tval(invalidNel[Int]("error1", "error2"))
  )

  private val combineTest = table("combine", combineTable)(t => t._1 combine t._2)  


  private val combineExtendedTable = truthTable(
    (valid(10), valid(20), valid(30))-> tval(valid(60)),
    (invalid[Int]("error1"), valid(20), valid(30))-> tval(invalid[Int]("error1")),
    (valid(10), invalid[Int]("error1"), valid(30))-> tval(invalid[Int]("error1")),
    (valid(10), valid(20), invalid[Int]("error1"))-> tval(invalid[Int]("error1")),
    (invalid[Int]("error1"), invalid[Int]("error2"), invalid[Int]("error3"))-> tval(invalidNel[Int]("error1", "error2", "error3"))
  )

  private val combineExtendedTest = table("combineExtended", combineExtendedTable)(t => t._1 combine t._2 combine t._3)

  private val combineKTable = truthTable(
    (valid(10), valid(20)) -> tval(valid(10)),
    (valid(10), invalid[Int]("error1")) -> tval(valid(10)),
    (invalid[Int]("error1"), valid(20)) -> tval(valid(20)),
    (invalid[Int]("error1"), invalid[Int]("error2")) -> tval(invalidNel[Int]("error1", "error2"))
  )

  //similar to x.getOrElse(y)
  private val combineKTest = table("combineK", combineKTable)(t => t._1 combineK t._2)


  private val productLTable = truthTable(
    (valid(10), valid(20)) -> tval(valid(10)),
    (valid(10), invalid[Int]("error1")) -> tval(invalid[Int]("error1")),
    (invalid[Int]("error1"), valid(20)) -> tval(invalid[Int]("error1")),
    (invalid[Int]("error1"), invalid[Int]("error2")) -> tval(invalidNel[Int]("error1", "error2"))
  )

  //product is similar to flatMap
  private val productLTest = table("productL", productLTable)(t => t._1 productL t._2)

  private val productRTable = truthTable(
    (valid(10), valid(20)) -> tval(valid(20)),
    (valid(10), invalid[Int]("error1")) -> tval(invalid[Int]("error1")),
    (invalid[Int]("error1"), valid(20)) -> tval(invalid[Int]("error1")),
    (invalid[Int]("error1"), invalid[Int]("error2")) -> tval(invalidNel[Int]("error1", "error2"))
  )

  private val productRTest = table("productR", productRTable)(t => t._1 productR t._2)

  override val tests = oneOrMore(combineTest, combineExtendedTest, combineKTest, productLTest, productRTest)

  private def valid[A](value: A): ValidatedNel[String, A] = value.validNel[String]
  
  private def invalid[A](error: String): ValidatedNel[String, A] = error.invalidNel[A] 
  
  private def invalidNel[A](error: String, others: String*): ValidatedNel[String, A] = Validated.invalid[NonEmptyList[String], A](NonEmptyList(error, others.toList)) 

}