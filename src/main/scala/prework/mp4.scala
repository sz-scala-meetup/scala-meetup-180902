package prework
import java.util.concurrent.ConcurrentHashMap
import monix.eval._
object mp4 extends App {
  class KVStore[K,V] {
    private val kvs = new ConcurrentHashMap[K,V]()
    def create(k: K, v: V): Task[Unit] = Task.delay(kvs.putIfAbsent(k,v))
    def read(k: K): Task[Option[V]] = Task.delay(Option(kvs.get(k)))
    def update(k: K, v: V): Task[Unit] = Task.delay(kvs.put(k,v))
    def delete(k: K): Task[Boolean] = Task.delay(kvs.remove(k) == null)
  }

  type FoodName = String
  type Quantity = Int
  type FoodStore = KVStore[String,Int]

  class Refridge {
    def addFood(food: FoodName, qty: Quantity): FoodStore => Task[Quantity] = { foodStore =>
      for {
        current <- foodStore.read(food)
        newQty = current.map(c => c + qty).getOrElse(qty)
        _ <- foodStore.update(food, newQty)
      } yield newQty
    }

    def takeFood(food: FoodName, qty: Quantity): FoodStore => Task[Quantity] = { foodStore =>
      for {
        current <- foodStore.read(food)
        cq = current.getOrElse(0)
        taken = Math.min(cq, qty)
        left = cq - taken
        _ <- if (left > 0) foodStore.update(food, left) else foodStore.delete(food)
      } yield taken
    }

  }

  class Cooker(store: Refridge) {
    def cookSauce(qty: Quantity): FoodStore => Task[Quantity] = { foodStore =>
      for {
        tomato <- store.takeFood("Tomato", qty)(foodStore)
        vaggies <- store.takeFood("Veggies", qty)(foodStore)
        _ <- store.takeFood("Galic", 10)(foodStore)
        sauceQ = tomato / 2 + vaggies * 2 / 3
        _ <- store.addFood("Sauce", sauceQ)(foodStore)

      } yield sauceQ
    }

    def cookPasta(qty: Quantity): FoodStore => Task[Quantity] = { foodStore =>
      for {
        pasta <- store.takeFood("Pasta", qty)(foodStore)
        sauce <- store.takeFood("Sauce", qty)(foodStore)
        _ <- store.takeFood("Spice", 3)(foodStore)
        portions = Math.min(pasta, sauce)
      } yield portions
    }
  }


  lazy val cooker = new Cooker(refridge)
  lazy val refridge = new Refridge
  lazy val foodStore = new FoodStore

  val shopping: Task[Unit] =  for {
    _ <- refridge.addFood("Tomato",10)(foodStore)
    _ <- refridge.addFood("Veggies",15)(foodStore)
    _ <- refridge.addFood("Garlic", 42)(foodStore)
    _ <- refridge.addFood("Spice", 100)(foodStore)
    _ <- refridge.addFood("Pasta", 6)(foodStore)
  } yield ()

  val cooking: Task[Quantity] = for {
    _ <- shopping
    sauce <- cooker.cookSauce(10)(foodStore)
    pasta <- cooker.cookPasta(10)(foodStore)
  } yield pasta

  import scala.concurrent.duration._
  import scala.util._
  import monix.execution.Scheduler.Implicits.global
  val timedCooking = cooking.timeoutTo(1 seconds, Task.raiseError( new RuntimeException(
    "oh no, take too long to cook ...")))
  val cancellableCooking = timedCooking.runOnComplete { result =>
    result match {
      case Success(meals) => println(s"we have $meals specials for the day.")
      case Failure(exception) => println(s"kitchen problem! ${exception.getMessage}")
    }
  }
  global.scheduleOnce(3 seconds) {
    println("3 seconds passed")
    cancellableCooking.cancel()
  }

  scala.io.StdIn.readLine()


}
