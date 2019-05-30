package week3.poly

import List._

object ListDemo extends App{

  val list = new Cons(1, new Cons(2, new Cons(3, new Nil)))

  println(list)
  println(list nth(1, list))
  println(list nth(-1, list))
}
