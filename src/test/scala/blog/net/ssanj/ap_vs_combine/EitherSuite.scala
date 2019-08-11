package blog.net.ssanj.ap_vs_combine

import boon._
import cats.implicits._

object EitherSuite extends SuiteLike("Either Suite") {

  //similar to x.getOrElse(y) when there are nones, but combines both if present, as `A` is a Semigroup/Monoid
  private val t1 = test("combine") {
    (right(10) combine right(20)) =?= right(30) | "combine Right and Right" and
    (right(10) combine left[Int]("error1")) =?= left[Int]("error1") | "combine Right and Left" and
    (left[Int]("error1") combine right(20)) =?= left[Int]("error1") | "combine Left and Right" and
    (left[Int]("error1") combine left[Int]("error2")) =?= left[Int]("error1") | "combine Left and Left" and
    (right(10) combine right(20) combine right(30)) =?= right(60) | "combine Right and Right and Right" and
    (left[Int]("error1") combine right(20) combine right(30)) =?= left[Int]("error1") | "combine Left and Right and Right" and
    (right(10) combine left[Int]("error1") combine right(30)) =?= left[Int]("error1") | "combine Right and Left and Right" and
    (right(10) combine right(20) combine left[Int]("error1")) =?= left[Int]("error1") | "combine Right and Left and Left"
  }

  // //similar to x.getOrElse(y)
  private val t2 = test("combineK") {
    (right(10) combineK right(20)) =?= right(10) | "combineK RightLeft and Right" and
    (right(10) combineK left[Int]("error1")) =?= right(10) | "combineK Right and Left" and
    (left[Int]("error1") combineK right(20)) =?= right(20) | "combineK Left and Right" and
    (left[Int]("error1") combineK left[Int]("error2")) =?= left[Int]("error2") | "combineK Left and Right"
  }

  // //product is similar to flatMap, in that any none leads to the full expression being none
  private val t3 = test("productL") {
    (right(10) productL right(20)) =?= right(10) | "productL Right and Right" and
    (right(10) productL left[Int]("error1")) =?= left[Int]("error1") | "productL Right and Left" and
    (left[Int]("error1") productL right(20)) =?= left[Int]("error1") | "productL Left and Right" and
    (left[Int]("error1") productL left[Int]("error2")) =?= left[Int]("error1") | "productL Left and Right"
  }

  private val t4 = test("productR") {
    (right(10) productR right(20)) =?= right(20) | "productR Right and Right" and
    (right(10) productR left[Int]("error1")) =?= left[Int]("error1") | "productR Right and Left" and
    (left[Int]("error1") productR right(20)) =?= left[Int]("error1") | "productR Left and Right" and
    (left[Int]("error1") productR left[Int]("error2")) =?= left[Int]("error1") | "productR Left and Right"
  }

  override val tests = oneOrMore(t1, t2 ,t3, t4)

  private def right[A](value: A): Either[String, A] = Right(value)
  
  private def left[A](error: String): Either[String, A] = Left(error)  
}