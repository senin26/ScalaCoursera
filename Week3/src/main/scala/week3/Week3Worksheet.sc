//object WSheet {
  println("Welcome")

  abstract class IntSet {
    def incl(x: Int): IntSet
    def contains(x: Int): Boolean
    def union(other: IntSet): IntSet
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

    override def union(other: IntSet): IntSet =
      ((left union right) union other) incl elem

    override def toString = "{" + left + elem + right + "}"
  }

  class Empty extends IntSet {
    def contains(x: Int): Boolean = false

    def incl(x: Int): IntSet = new NonEmpty(x, new Empty, new Empty)

    def union(other: IntSet): IntSet = other

    override def toString = "."
  }

  val t1 = new NonEmpty(3, new Empty, new Empty)
  val t2 = t1 incl 4
/*
}
import WSheet.{Empty, NonEmpty}
val t1 = new NonEmpty(3, new Empty, new Empty)
val t2 = t1 incl 4*/
/*def error(msg: String) = throw new Error(msg)
error("Fucking error")
val x = null
val y: String = x
val z: Int = x*/
if (true) 1 else false

