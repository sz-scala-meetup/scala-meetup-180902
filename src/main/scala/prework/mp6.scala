package prework
import java.util.concurrent.ConcurrentHashMap
import monix.eval._
import cats._
import data._
import implicits._
object mp6 {

  trait DBTable[K,V] {
    def create(k: K, v: V): Task[Unit]

    def read(k: K): Task[Option[V]]

    def update(k: K, v: V): Task[Unit]

    def delete(k: K): Task[Unit]
  }
  class KVStore[K, V] extends DBTable[K,V] {
    private val kvs = new ConcurrentHashMap[K, V]()

    def create(k: K, v: V): Task[Unit] = Task.delay(kvs.putIfAbsent(k, v))

    def read(k: K): Task[Option[V]] = Task.delay(Option(kvs.get(k)))

    def update(k: K, v: V): Task[Unit] = Task.delay(kvs.put(k, v))

    def delete(k: K): Task[Unit] = Task.delay(kvs.remove(k))
  }


  type FoodName = String
  type Quantity = Int
  type FoodStore = DBTable[String, Int]
  type TResult[T] = ReaderT[Task,FoodStore,T]

  class Refridge {
    def addFood(food: FoodName, qty: Quantity): TResult[Quantity] = ReaderT { store => {
      for {
        current <- store.read(food)
        updated = current.map(c => c + qty).getOrElse(qty)
        _ <- store.update(food, updated)
      } yield updated
    }
    }


    def takeFood(food: FoodName, qty: Quantity): TResult[Quantity] = ReaderT { store => {
      for {
        current <- store.read(food)
        curQ = current.getOrElse(0)
        taken = Math.min(curQ, qty)
        left = curQ - taken
        _ <- if (left > 0) store.update(food, left) else store.delete(food)
      } yield taken
    }
    }
  }

  class Cooker(store: Refridge) {
    def cookSauce(qty: Quantity): TResult[Quantity]  = ReaderT { foodStore => {
      for {
        tomato <- store.takeFood("Tomata", qty)(foodStore)
        veggies <- store.takeFood("Veggies", qty)(foodStore)
        _ <- store.takeFood("Galic", qty / 2)(foodStore)
        sauceQ = tomato / 2 + veggies * 4 / 3
        _ <- store.addFood("Sauce", sauceQ)(foodStore)
      } yield sauceQ
    }
    }


    def cookPasta(qty: Quantity): TResult[Quantity]  =  ReaderT { foodStore => {
      for {
        pastaQ <- store.takeFood("Pasta", qty)(foodStore)
        sauceQ <- store.takeFood("Sauce", qty)(foodStore)
        _ <- store.takeFood("Spice", 10)(foodStore)
        portions = Math.min(pastaQ, sauceQ)
        _ <- store.addFood("Meal", portions)(foodStore)
      } yield portions
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


  // val timedCooking = prepareMeals.run(fc).timeoutTo(3 hours, Task.raiseError(new Exception("oh, it's fcking too long!")))
  val cancellableCooking = prepareMeals.run(fc).runOnComplete {
    case Success(meals) => println(s"we have $meals specials for today.")
    case Failure(exception) => println(s"kitchen on fire! reason: ${exception.getMessage}")
  }

  global.scheduleOnce(3 seconds) {
    println("cooking time is over!!!")
    cancellableCooking.cancel()
  }

  scala.io.StdIn.readLine()
}
