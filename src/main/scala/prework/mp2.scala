package prework
import java.util.concurrent.ConcurrentHashMap
import monix.eval._
import scala.concurrent.duration._
object mp2 extends App {

  class KVStore[K,V] {
    private val kvs = new ConcurrentHashMap[K,V]()
    def create(k: K, v: V): Task[Unit] = Task.delay(kvs.putIfAbsent(k,v))
    def read(k: K): Task[Option[V]] = Task.delay(Option(kvs.get(k)))
    def update(k: K, v: V): Task[Unit] = Task.delay(kvs.put(k,v))
    def delete(k: K): Task[Boolean] = Task.delay(kvs.remove(k) != null)
  }

  type FoodName = String
  type Quantity = Int
  type FoodStore = KVStore[String,Int]

  def addFood(food: FoodName, qty: Quantity)(implicit fs: FoodStore): Task[Quantity] = for {
    current <- fs.read(food)
    newQty = current.map(cq => cq + qty).getOrElse(qty)
    _ <- fs.update(food,newQty)
  } yield newQty

  def takeFood(food: FoodName, qty: Quantity)(implicit fs: FoodStore): Task[Quantity] = for {
    current <- fs.read(food)
    cq = current.getOrElse(0)
    taken = Math.min(cq,qty)
    left = cq - taken
    _ <- if(left > 0) fs.update(food,left) else fs.delete(food)
  } yield taken


  def cookSauce(qty: Quantity)(get: (FoodName,Quantity) => Task[Quantity],
                               put: (FoodName,Quantity) => Task[Quantity]): Task[Quantity] = for {
    tomato <- get("Tomato",qty)
    vaggies <- get("Veggies",qty)
    _ <- get("Galic",10)
    sauceQ = tomato/2 + vaggies * 3 / 2
    _ <- put("Sauce",sauceQ)
  } yield sauceQ

  def cookPasta(qty: Quantity)(get: (FoodName,Quantity) => Task[Quantity],
                               put: (FoodName,Quantity) => Task[Quantity]): Task[Quantity] = for {
    pasta <- get("Pasta", qty)
    sauce <- get("Sauce", qty)
    _ <- get("Spice", 3)
    portions = Math.min(pasta, sauce)
    _ <- put("Meal", portions)
  } yield portions


  implicit val refridge = new FoodStore

  val shopping: Task[Unit] = for {
    _ <- addFood("Tomato",10)
    _ <- addFood("Veggies",15)
    _ <- addFood("Garlic", 42)
    _ <- addFood("Spice", 100)
    _ <- addFood("Pasta", 6)
  } yield()

  val cooking: Task[Quantity] = for {
    _ <- shopping
    sauce <- cookSauce(10)(takeFood(_,_),addFood(_,_))
    meals <- cookPasta(10)(takeFood(_,_),addFood(_,_))
  } yield meals


  val timedCooking = cooking.timeoutTo(500 millis,Task.raiseError(new RuntimeException(
    "oh my, too fcking long..."
  )))

  import scala.util._
  import monix.execution.Scheduler.Implicits.global

  val cancellableCooking = timedCooking.runOnComplete { result =>
    result match {
      case Success(meals) => println(s"we have $meals pasta meals for the day.")
      case Failure(err) => println(s"cooking trouble: ${err.getMessage}")
    }
  }

  global.scheduleOnce(1 second) {
    println(s"its taking too long, cancelling cooking ...")
    cancellableCooking.cancel()
  }

  scala.io.StdIn.readLine()

}
