package practice

object lesson1 extends App {

  case class Tube[A](run: A) {
    def map[B](f: A => B): Tube[B] = Tube(f(run))
    def flatMap[B](f: A => Tube[B]): Tube[B] = f(run)
  }

  def boxedValue(a: Int): Tube[Int] = Tube(a)
  def add(a: Int, b: Int): Tube[Int] = Tube(a + b)
  val sum: Tube[Int] = for {
    a <- boxedValue(10)
    b <- boxedValue(20)
    c <- add(a,b)
  } yield c
  println(sum)
  println(sum.run)

}
