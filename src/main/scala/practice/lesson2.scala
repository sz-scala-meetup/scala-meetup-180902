package practice
object lesson2 extends App {
  val value: Option[Int] = Some(10)
  def add(a: Int, b: Int): Option[Int] = Some(a+b)

  val p = for {
    a <- value
    b <- add(a, 3)
    _ <- None
    c <- add(a,b)
  } yield a

  println(p)

}

object lesson21 extends App {
  val value: Either[String,Int] = Right(10)
  def add(a: Int, b: Int): Either[String,Int] = Right(a+b)

  val p = for {
    a <- value
    b <- add(a, 3)
    _ <- Left("oh no ...")
    c <- add(a,b)
  } yield c

  println(p)

}