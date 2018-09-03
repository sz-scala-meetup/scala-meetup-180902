package practice
import cats._
import implicits._
import monix.eval._
import data._
import monix.execution.Scheduler.Implicits.global
import scala.util._

object lesson3 extends App  {
  type OResult[T] = OptionT[Task,T]

  def add(a: Int, b: Int): Task[Int] = Task.delay(a + b)

  def valueToOResult[A](a: A): OResult[A] = Applicative[OResult]pure(a)
  def optionToOResult[A](a: Option[A]): OResult[A] = OptionT(a.pure[Task])
  def taskToOResult[A](a: Task[A]): OResult[A] = OptionT.liftF(a)

  val sum: OResult[Int] = for {
     a <- valueToOResult(10)
     b <- optionToOResult(Some(10))   //None: Option[Int])     //Some(10))
     c <- taskToOResult(add(a,b))
  } yield c

  sum.value.runOnComplete {
    case Success(value) => println(s"the sum of OptionT is : $value")
    case Failure(exception) => println(exception.getMessage)
  }

}

object lesson31 extends App {
  type EResult[T] = EitherT[Task,String,T]
  def add(a: Int, b: Int): Task[Int] = Task.delay(a + b)

  def valueToEResult[A](a: A): EResult[A] = Applicative[EResult].pure(a)
  def eitherToEResult[A](a: Either[String,A]): EResult[A] = EitherT(a.pure[Task])
  def taskToEResult[A](a: Task[A]): EResult[A] = EitherT.liftF(a)

  val calc: EResult[Int] = for {
    a <- valueToEResult(10)
    b <- eitherToEResult(Left[String,Int]("oh, my good ..."))  //    Right(5))
    c <- taskToEResult(add(a,b))
  } yield c

  val sum = calc.value
  sum.runOnComplete {
    case Success(value) => println(s"the sum of EitherT is : $value")
    case Failure(exception) => println(exception.getMessage)
  }
}