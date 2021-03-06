package week3

object IntSetDemo extends App{

  val t1 = new NonEmpty(3, new Empty, new Empty)
  val t2 = t1 incl 4

  println(t1)
  println(t2)

  class Empty extends IntSet {
    def contains(x: Int): Boolean = false

    def incl(x: Int): IntSet = new NonEmpty(x, new Empty, new Empty)

    override def toString = "."
  }

  class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {

    override def contains(x: Int): Boolean =
      if (x < elem) left contains x
      else if (x > elem) right contains x
      else true

    override def incl(x: Int): IntSet =
      if (x < elem) new NonEmpty(elem, left incl x, right)
      else if (x > elem) new NonEmpty(elem, left, right incl x)
      else this

    override def toString = "{" + left + elem + right + "}"
  }

  abstract class IntSet {
    def incl(x: Int): IntSet

    def contains(x: Int): Boolean
  }


}
