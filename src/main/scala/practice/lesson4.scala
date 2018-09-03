package practice
import cats._
import implicits._
import data._
import monix.eval._
import monix.execution.Scheduler.Implicits.global
import scala.util._

object lesson4 extends App {
  type DBOError[T] = EitherT[Task,String,T]
  type DBOResult[T] = OptionT[DBOError,T]

  def valueToDBOResult[A](a: A): DBOResult[A] = Applicative[DBOResult].pure(a)
  def optionToDBOResult[A](a: Option[A]): DBOResult[A] = OptionT(a.pure[DBOError])
  def eitherToDBOResult[A](a: Either[String,A]): DBOResult[A] = {
    val error: DBOError[A] = EitherT.fromEither(a)
    OptionT.liftF(error)
  }
  def taskToDBOResult[A](task: Task[A]): DBOResult[A] = {
    val error: DBOError[A] = EitherT.liftF[Task,String,A](task)
    OptionT.liftF(error)
  }

  def add(a: Int, b: Int): Task[Int] = Task.delay(a + b)

  val calc: DBOResult[Int] = for {
    a <- valueToDBOResult(10)
    b <- optionToDBOResult(None: Option[Int])  //Some(3))
    c <- eitherToDBOResult(Left[String,Int]("Oh my ...")) //    Right(7))
    d <- taskToDBOResult(add(b,c))
  } yield d

  val sum = calc.value.value
  sum.runOnComplete {
    case Success(value) => println(s"the sum of DBOResult is : $value")
    case Failure(exception) => println(exception.getMessage)
  }

}
