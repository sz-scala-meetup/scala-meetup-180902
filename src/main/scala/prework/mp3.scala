package prework
import java.util.concurrent.ConcurrentHashMap
import monix.eval._
object mp3 extends App {
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

  class Refridge(kvs: FoodStore) {
    def addFood(food: FoodName, qty: Quantity): Task[Quantity] = for {
      current <- kvs.read(food)
      newQty = current.map(c => c + qty).getOrElse(qty)
      _ <- kvs.update(food,newQty)
    } yield newQty

    def takeFood(food: FoodName, qty: Quantity): Task[Quantity] = for {
      current <- kvs.read(food)
      cq = current.getOrElse(0)
      taken = Math.min(cq,qty)
      left = cq - taken
      _ <- if(left > 0) kvs.update(food,left) else kvs.delete(food)
    } yield taken

  }

  class Cooker(store: Refridge) {
    def cookSauce(qty: Quantity): Task[Quantity] = for {
      tomato <- store.takeFood("Tomato",qty)
      vaggies <- store.takeFood("Veggies",qty)
      _ <- store.takeFood("Galic",10)
      sauceQ = tomato/2 + vaggies * 2 / 3
      _ <- store.addFood("Sauce", sauceQ)

    } yield sauceQ

    def cookPasta(qty: Quantity): Task[Quantity] = for {
      pasta <- store.takeFood("Pasta",qty)
      sauce <- store.takeFood("Sauce",qty)
      _ <- store.takeFood("Spice",3)
      portions = Math.min(pasta,sauce)
    } yield portions
  }

  lazy val cooker = new Cooker(refridge)
  lazy val refridge = new Refridge(foodStore)
  lazy val foodStore = new FoodStore

  val shopping: Task[Unit] =  for {
    _ <- refridge.addFood("Tomato",10)
    _ <- refridge.addFood("Veggies",15)
    _ <- refridge.addFood("Garlic", 42)
    _ <- refridge.addFood("Spice", 100)
    _ <- refridge.addFood("Pasta", 6)
  } yield ()

  val cooking: Task[Quantity] = for {
    _ <- shopping
    sauce <- cooker.cookSauce(10)
    pasta <- cooker.cookPasta(10)
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
