package practice
import java.util.concurrent.ConcurrentHashMap

import cats._
import data._
import implicits._
import monix.eval._

object DBOResult  {
  type DBOError[A] = EitherT[Task,String,A]
  type DBOResult[A] = OptionT[DBOError,A]

  def valueToDBOResult[A](a: A): DBOResult[A] =
    Applicative[DBOResult].pure(a)
  def optionToDBOResult[A](a: Option[A]): DBOResult[A] =
    OptionT(a.pure[DBOError])
  def eitherToDBOResult[A](a: Either[String,A]): DBOResult[A] = {
    val error: DBOError[A] = EitherT.fromEither(a)
    OptionT.liftF(error)
  }
  def taskToDBOResult[A](task: Task[A]): DBOResult[A] = {
    val error: DBOError[A] = EitherT.liftF(task)
    OptionT.liftF((error))
  }

  implicit class DBOResultToTask[A](r: DBOResult[A]) {
    def result = r.value.value
  }

}

object lesson5 extends App {
import DBOResult._

  trait DBTable[K,V] {
    def create(k: K, v: V): DBOResult[Unit]

    def read(k: K): DBOResult[V]

    def update(k: K, v: V): DBOResult[Unit]

    def delete(k: K): DBOResult[Unit]
  }

  class KVStore[K, V] extends DBTable[K,V] {
    private val kvs = new ConcurrentHashMap[K, V]()

    def create(k: K, v: V): DBOResult[Unit] =
      try {
        valueToDBOResult(kvs.putIfAbsent(k, v))
      } catch {
        case e: Exception => eitherToDBOResult(Left[String, Unit](e.getMessage))
      }

    def read(k: K): DBOResult[V] =
      try {
        if (kvs.isEmpty)
          //optionToDBOResult(None: Option[V])
           valueToDBOResult(0.asInstanceOf[V])
        else
            valueToDBOResult(kvs.get(k))
      } catch {
        case e: Exception => eitherToDBOResult(Left[String, V](e.getMessage))
      }

    def update(k: K, v: V): DBOResult[Unit] =
      try {
        valueToDBOResult(kvs.put(k, v))
      } catch {
        case e: Exception => eitherToDBOResult(Left[String, Unit](e.getMessage))
      }

    def delete(k: K): DBOResult[Unit] =
      try {
        valueToDBOResult(kvs.remove(k))
      } catch {
        case e: Exception => eitherToDBOResult(Left[String, Unit](e.getMessage))
      }

  }


  type FoodName = String
  type Quantity = Int
  type FoodStore = DBTable[String, Int]
  type TResult[T] = ReaderT[DBOResult,FoodStore,T]

  class Refridge {
    def addFood(food: FoodName, qty: Quantity): TResult[Quantity] = ReaderT { store => {
      val r: DBOResult[Quantity] = for {
        current <- store.read(food)
        updated = current + qty
        _ <- store.update(food, updated)
      } yield updated
      r
    }
    }


    def takeFood(food: FoodName, qty: Quantity): TResult[Quantity] = ReaderT { store => {
      val r: DBOResult[Quantity] = for {
        current <- store.read(food)
        curQ = current
        taken = Math.min(curQ, qty)
        left = curQ - taken
        _ <- if (left > 0) store.update(food, left) else store.delete(food)
      } yield taken
      r
    }
    }
  }

  class Cooker(store: Refridge) {
    def cookSauce(qty: Quantity): TResult[Quantity]  = ReaderT { foodStore => {
      val r: DBOResult[Quantity] = for {
        tomato <- store.takeFood("Tomata", qty)(foodStore)
        veggies <- store.takeFood("Veggies", qty)(foodStore)
        _ <- store.takeFood("Galic", qty / 2)(foodStore)
        sauceQ = tomato / 2 + veggies * 4 / 3
        _ <- store.addFood("Sauce", sauceQ)(foodStore)
      } yield sauceQ
      r
    }
    }


    def cookPasta(qty: Quantity): TResult[Quantity]  =  ReaderT { foodStore => {
      val r: DBOResult[Quantity] = for {
        pastaQ <- store.takeFood("Pasta", qty)(foodStore)
        sauceQ <- store.takeFood("Sauce", qty)(foodStore)
        _ <- store.takeFood("Spice", 10)(foodStore)
        portions = Math.min(pastaQ, sauceQ)
        _ <- store.addFood("Meal", portions)(foodStore)
      } yield portions
      r
    }
    }
  }

  lazy val cooker = new Cooker(refridge)
  lazy val refridge = new Refridge
  lazy val fc = new KVStore[String,Int]

  val shopping: TResult[Unit]  = for {
    _ <- refridge.addFood("Tomato", 10)
    _ <- refridge.addFood("Veggies", 15)
    _ <- refridge.addFood("Galic", 10)
    _ <- refridge.addFood("Spice", 100)
    _ <- refridge.addFood("Pasta", 8)
  } yield ()

  val prepareMeals: TResult[Quantity]  = for {
    _ <- shopping
    sauce <- cooker.cookSauce(10)
    meals <- cooker.cookPasta(10)
  } yield meals

  import monix.execution.Scheduler.Implicits.global

  import scala.concurrent.duration._
  import scala.util._

  val task = prepareMeals.run(fc).result
  // val timedCooking = prepareMeals.run(fc).timeoutTo(3 hours, Task.raiseError(new Exception("oh, it's fcking too long!")))


  val cancellableCooking = task.runOnComplete {
    case Success(meals) => println(s"we have $meals specials for today.")
    case Failure(exception) => println(s"kitchen on fire! reason: ${exception.getMessage}")
  }

  global.scheduleOnce(3 seconds) {
    println("cooking time is over!!!")
    cancellableCooking.cancel()
  }

  scala.io.StdIn.readLine()
}
