package blog.net.ssanj.ap_vs_combine

import boon._
import cats.effect.IO
import cats.implicits._

object IOSuite extends SuiteLike("IO Suite") {

  //temporary wrapper around IO as boon does not support side effects.
  sealed trait IOType[A]
  final case class IOSuccess[A](value: A) extends IOType[A]
  final case class IOError[A](value: String) extends IOType[A]

  implicit def ioTypeBoonType[A] = BoonType.defaults[IOType[A]]

  private   val combineTable = truthTable(
    (success(10), success(20)) -> tval(success(30)),
    (success(10), error[Int]("error1")) -> tval(error[Int]("error1")),
    (error[Int]("error1"), success(20)) -> tval(error[Int]("error1")),
    (error[Int]("error1"), error[Int]("error2")) -> tval(error[Int]("error1"))
  )

  private val combineTest = table("combine", combineTable)(io(_ combine _))

  private val combineKTable = truthTable(
    (success(10), success(20)) -> tval(success(10)),
    (success(10), error[Int]("error1")) -> tval(success(10)),
    (error[Int]("error1"), success(20)) -> tval(success(20)),
    (error[Int]("error1"), error[Int]("error2")) -> tval(error[Int]("error2"))
  )

  //similar to x.getOrElse(y)
  private val combineKTest = table("combineK", combineKTable)(io(_ combineK _))


  private val productLTable = truthTable(
    (success(10), success(20)) -> tval(success(10)),
    (success(10), error[Int]("error1")) -> tval(error[Int]("error1")),
    (error[Int]("error1"), success(20)) -> tval(error[Int]("error1")),
    (error[Int]("error1"), error[Int]("error2")) -> tval(error[Int]("error1"))
  )

  //product is similar to flatMap
  private val productLTest = table("productL", productLTable)(io(_ productL _))

  private val productRTable = truthTable(
    (success(10), success(20)) -> tval(success(20)),
    (success(10), error[Int]("error1")) -> tval(error[Int]("error1")),
    (error[Int]("error1"), success(20)) -> tval(error[Int]("error1")),
    (error[Int]("error1"), error[Int]("error2")) -> tval(error[Int]("error1"))
  )

  private val productRTest = table("productR", productRTable)(io(_ productR _))

  override val tests = oneOrMore(combineTest, combineKTest ,productLTest, productRTest)

  private def io[A, B, C](f: (IO[A], IO[B]) => IO[C])(t: Tuple2[IOType[A], IOType[B]]): IOType[C] = (t._1, t._2) match {
    case (IOSuccess(a), IOSuccess(b)) =>  runToIOType(f(IO(a), IO(b)))
    case (IOError(error), IOSuccess(b)) => runToIOType(f(IO.raiseError[A](new RuntimeException(error)), IO(b)))
    case (IOSuccess(a), IOError(error)) => runToIOType(f(IO(a), IO.raiseError[B](new RuntimeException(error))))
    case (IOError(error1), IOError(error2)) => runToIOType(f(IO.raiseError[A](new RuntimeException(error1)), IO.raiseError[B](new RuntimeException(error2))))
  }

  private def runIO[A](io: IO[A]): Either[Throwable, A] = io.attempt.unsafeRunSync
  
  private def runToIOType[A] = attemptToIOType[A] _ compose runIO[A] _
  
  private def attemptToIOType[A](attempt: Either[Throwable, A]): IOType[A] = attempt.fold[IOType[A]](l => IOError[A](l.getMessage), IOSuccess[A](_))

  private def success[A](value: A): IOType[A] = IOSuccess(value)
  
  private def error[A](error: String): IOType[A] = IOError(error)
}















