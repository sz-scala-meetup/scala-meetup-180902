package prework
import scala.concurrent._
import java.util.concurrent.ConcurrentHashMap
import scala.concurrent.duration._

object mp1 extends App {
  import scala.concurrent.ExecutionContext.Implicits.global

  class KVStore[K,V] {
    private val s = new ConcurrentHashMap[K,V]()
    def create(k: K, v: V): Future[Boolean] = Future.successful(s.putIfAbsent(k,v) == null)
    def read(k: K): Future[Option[V]] = Future.successful(Option(s.get(k)))
    def update(k: K, v: V): Future[Unit] = Future.successful(s.put(k,v))
    def delete(k: K): Future[Boolean] = Future.successful(s.remove(k) == null)
  }

  type FoodName = String
  type Quantity = Int
  type FoodStore = KVStore[String,Int]

  def addFood(food: FoodName, qty: Quantity )(implicit fs: FoodStore): Future[Unit] = for {
    current <- fs.read(food)
    newQty = current.map(cq => cq + qty ).getOrElse(qty)
    _ <-  fs.update(food, newQty)
  } yield ()

  def takeFood(food: FoodName, qty: Quantity)(implicit fs: FoodStore): Future[Quantity] = for {
    current <- fs.read(food)
    instock = current.getOrElse(0)
    taken = Math.min(instock,qty)
    left = instock - taken
    _ <- if (left > 0) fs.update(food,left) else fs.delete(food)
  } yield taken

  def cookSauce(qty: Quantity)(get: (FoodName,Quantity) => Future[Quantity],
                               put:(FoodName,Quantity) => Future[Unit]): Future[Quantity] = for {
    tomato <- get("Tomato",qty)
    veggie <- get("Veggie",qty)
    garlic <- get("Garlic", qty * 3)
    sauceQ = tomato / 2 + veggie * 3 / 2
    _ <- put("Sauce",sauceQ)
  } yield sauceQ

  def cookMeals(qty: Quantity)(get: (FoodName,Quantity) => Future[Quantity],
                               put: (FoodName,Quantity) => Future[Unit]): Future[Quantity] =
    for {
       pasta <- get("Pasta", qty)
       sauce <- get("Sauce", qty)
      _ <- get("Spice",10)

      meals = Math.min(pasta,sauce)
      _ <- put("Meal", meals)

    } yield meals

   implicit val refrigerator = new FoodStore

   val shopping: Future[Unit] = for {
     _ <- addFood("Tomato", 10)
     _ <- addFood("Veggie", 15)
     _ <- addFood("Garlic", 42)
     _ <- addFood("Spice", 100)
     _ <- addFood("Pasta", 6)
   } yield ()

   val cooking: Future[Quantity] = for {
     _ <- shopping
     sauce <- cookSauce(10)(takeFood(_,_),addFood(_,_))
     meals <- cookMeals(10)(takeFood(_,_),addFood(_,_))
   } yield (meals)

   val todaysMeals = Await.result(cooking,3 seconds)

  println(s"we have $todaysMeals pasta meals for the day.")

}
