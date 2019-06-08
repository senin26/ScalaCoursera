package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
    val l = List('a', 'b', 'a')
    val m = List('a', 'b', 'a', 'b', 'a', 's', 'a')
    val lPairs = List(('a', 4), ('b', 2), ('s', 1))

    // CodeTree #1
    val leaf1_1 = Leaf('a', 2)
    val leaf2_1 = Leaf('b', 1)
    val leaf3_1 = Leaf('c', 3)
    val fork1_1 = Fork(leaf1_1, leaf2_1, List(leaf1_1.char, leaf2_1.char), leaf1_1.weight+leaf2_1.weight)
    val cd_1 = Fork(fork1_1, leaf3_1, fork1_1.chars:::List(leaf3_1.char), fork1_1.weight+leaf3_1.weight)

    // CodeTree #2
    val leaf1_2 = Leaf('d', 4)
    val leaf2_2 = Leaf('e', 2)
    val leaf3_2 = Leaf('f', 1)
    val fork1_2 = Fork(leaf1_2, leaf2_2, List(leaf1_2.char, leaf2_2.char), leaf1_2.weight+leaf2_2.weight)
    val cd_2 = Fork(leaf3_2, fork1_2, List(leaf3_2.char):::fork1_2.chars, fork1_2.weight+leaf3_2.weight)

    // CodeTree #3
    val cd_3 = Fork(Leaf('g', 5), Leaf('h', 3), List('g', 'h'), 8)

  }


  /*  test("weight of a larger tree") {
      new TestTrees {
        assert(weight(t1) === 5)
      }
    }


    test("chars of a larger tree") {
      new TestTrees {
        assert(chars(t2) === List('a','b','d'))
      }
    }

    // additional
    test("check for def pair  a") {
      new TestTrees {
        assert(pair(l, 'a') ===  ('a', 2))
      }
    }

    // additional
    test("check for def pair b") {
      new TestTrees {
        assert(pair(l, 'b') ===  ('b', 1))
      }
    }

    // additional
    test("check for def times") {
      new TestTrees {
        assert(times(l) ===  List(('a', 2), ('b', 1)))
      }
    }

    test("check for def times 2") {
      new TestTrees {
        assert(times(m) ===  List(('a', 4), ('b', 2), ('s', 1)))
      }
    }*/
  /*
    test("check for makeOrderedLeafList") {
      new TestTrees {
        assert(makeOrderedLeafList(lPairs) === List(Leaf('a', 4), Leaf('b', 2), Leaf('s', 1)))
      }
    }*/

  /* test("string2chars(\"hello, world\")") {
     assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
   }

   test("makeOrderedLeafList for some frequency table") {
     assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
   }*/

  /*test("singleton true") {
    assert(singleton(List(Leaf('a', 1))) === true)
  }

  test("singleton false") {
    assert(singleton(List(Leaf('a', 1), Leaf('b', 2))) === false)
  }*/

  /* test("list test") {
     val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
     assert(listTest(leaflist) === List(1, 2, 4))
   }*/

  /*  test("combine of some leaf list") {
     val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
     assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
   }*/

  /*test("combine of some leaf list") {
    // CodeTree #1
    val leaf1_1 = Leaf('a', 2)
    val leaf2_1 = Leaf('b', 1)
    val leaf3_1 = Leaf('c', 3)
    val fork1_1 = Fork(leaf1_1, leaf2_1, List(leaf1_1.char, leaf2_1.char), leaf1_1.weight+leaf2_1.weight)
    val cd_1 = Fork(fork1_1, leaf3_1, fork1_1.chars:::List(leaf3_1.char), fork1_1.weight+leaf3_1.weight) // total weight 6
    // CodeTree #2
    val leaf1_2 = Leaf('d', 4)
    val leaf2_2 = Leaf('e', 2)
    val leaf3_2 = Leaf('f', 1)
    val fork1_2 = Fork(leaf1_2, leaf2_2, List(leaf1_2.char, leaf2_2.char), leaf1_2.weight+leaf2_2.weight)
    val cd_2 = Fork(leaf3_2, fork1_2, List(leaf3_2.char):::fork1_2.chars, fork1_2.weight+leaf3_2.weight) // total weight 7
    // CodeTree #3
    val cd_3 = Fork(Leaf('g', 5), Leaf('h', 3), List('g', 'h'), 18) // total weight 7


    //assert(forktwoTrees(cd_1, cd_2) === Fork(cd_1, cd_2, List('a', 'b', 'c', 'f', 'd', 'e'), 13)) +

    //assert(combine2(List(cd_1, cd_2, cd_3)) === List(cd_3, Fork(cd_1, cd_2, List('a', 'b', 'c', 'f', 'd', 'e'), 13))) //  with sort under the hood
    assert(combine(List(cd_1, cd_2, cd_3)) === List(Fork(cd_1, cd_2, List('a', 'b', 'c', 'f', 'd', 'e'), 13), cd_3)) //  with sort under the hood

    //assert(filterTrees(List(cd_2, cd_3, cd_1)) === List(cd_1, cd_2, cd_3)) // try with bigger weight of cd_3 +
    //assert(filterTrees(combine2(List(cd_1, cd_2, cd_3))) === List(cd_3, Fork(cd_1, cd_2, List('a', 'b', 'c', 'f', 'd', 'e'), 13))) // try with bigger weight of cd_3 +
  }*/


  /*  test("until function") {

      // CodeTree #1
      val leaf1_1 = Leaf('a', 2)
      val leaf2_1 = Leaf('b', 1)
      val leaf3_1 = Leaf('c', 3)
      val fork1_1 = Fork(leaf1_1, leaf2_1, List(leaf1_1.char, leaf2_1.char), leaf1_1.weight+leaf2_1.weight)
      val cd_1 = Fork(fork1_1, leaf3_1, fork1_1.chars:::List(leaf3_1.char), fork1_1.weight+leaf3_1.weight) // total weight 6
      // CodeTree #2
      val leaf1_2 = Leaf('d', 4)
      val leaf2_2 = Leaf('e', 2)
      val leaf3_2 = Leaf('f', 1)
      val fork1_2 = Fork(leaf1_2, leaf2_2, List(leaf1_2.char, leaf2_2.char), leaf1_2.weight+leaf2_2.weight)
      val cd_2 = Fork(leaf3_2, fork1_2, List(leaf3_2.char):::fork1_2.chars, fork1_2.weight+leaf3_2.weight) // total weight 7
      // CodeTree #3
      val cd_3 = Fork(Leaf('g', 5), Leaf('h', 3), List('g', 'h'), 8) // total weight 7

     // val cd_list = List(cd_1, cd_2)

     // assert(combine(cd_list) === List(Fork(cd_1, cd_2, List('a', 'b', 'c', 'f', 'd', 'e'), 13))) //  with sort under the hood
      //assert(until(x => singleton(x), x => combine(x))(List(cd_1, cd_2)) === List(Fork(cd_1, cd_2, List('a', 'b', 'c', 'f', 'd', 'e'), 13))) //  with sort under the hood
      //assert(until(x => singleton(x), x => combine(x))(List(cd_1)) === List(cd_1)) //  with sort under the hood
      //assert(until(x => singleton(x), x => combine(x))(List(cd_1, cd_2)) === List(Fork(cd_1, cd_2, List('a', 'b', 'c', 'f', 'd', 'e'), 13))) //  with sort under the hood ++
      assert(until(x => singleton(x), x => combine(x))(List(cd_1, cd_2, cd_3)) === List(Fork(Fork(cd_1, cd_2, List('a', 'b', 'c', 'f', 'd', 'e'), 13), cd_3, List('a', 'b', 'c', 'f', 'd', 'e', 'g', 'h'), 21))) //  with sort under the hood ++
    }*/

  /*test("check divide on two") {
    new TestTrees {
      val text = "This is a mezzage"   // val text = "This is a fucking important message"
      val charList: List[Char]  = text.toList
      assert(createCodeTree(charList) === true)
    }
  }

  test("check creation of the tree") {
    new TestTrees {
      val text = "This is a mezzage"   // val text = "This is a fucking important message"
      val charList: List[Char]  = text.toList
      assert(createCodeTree(charList) === true)
    }
  }*/

  /*  test("check getAmountCharInList") {
      new TestTrees {
        val text = "This is a mezzage"   // val text = "This is a fucking important message"
        val charList: List[Char]  = text.toList
        assert(getAmountCharInList(charList, ' ') === 3)
      }
    }*/

  /*  test("charList sort") {
      new TestTrees {
        val text = "text"   // val text = "This is a fucking important message"
        val charList: List[Char]  = text.toList
        assert(sortCharList(charList) === "ettx".toList)
      }
    }*/

  test("get left char list") {
    new TestTrees {
      val text = "text"   // val text = "This is a fucking important message"
      val charList: List[Char]  = text.toList
      assert(getLeftCharList(sortCharList(charList)) === "ett".toList)
    }
  }

  test("get right char list") {
    new TestTrees {
      val text = "text"   // val text = "This is a fucking important message"
      val charList: List[Char]  = text.toList
      assert(getRightCharList(sortCharList(charList)) === "x".toList)
    }
  }

  test("all chars are same") {
    new TestTrees {
      val text = "tttt"   // val text = "This is a fucking important message"
      val charList: List[Char]  = text.toList
      assert(allCharsAreSame(charList) === true)
    }
  }

  /*  test("sort single char") { //this test is ok
      new TestTrees {
        val text = ""   // val text = "This is a fucking important message"
        val charList: List[Char]  = text.toList
        assert(sortCharList(charList) === "".toList)
      }
    }*/

  test("create code tree ") {
    new TestTrees {
      val text = "texxt"   // val text = "This is a fucking important message"
      val charList: List[Char]  = text.toList
      // CodeTree #1
      val leaf1 = Leaf('e', 1)
      val leaf2 = Leaf('t', 2)
      val leaf3 = Leaf('x', 2)
      val fork1 = Fork(leaf1, leaf2, List(leaf1.char, leaf2.char), leaf1.weight+leaf2.weight)
      val fork2 = Fork(fork1, leaf3, List(leaf1.char, leaf2.char, leaf3.char), fork1.weight+leaf3.weight)

      assert(createCodeTree(charList) === fork2)
    }
  }


  // getAmountCharInList(list: List[Char], char: Char)


  /*  test("check empty tail") {
      new TestTrees {
        val list = List(cd_1, cd_2)
        assert(myFun(list).isEmpty === true)
      }
    }*/


  /*
    test("decode and encode a very short text should be identity") {
      new TestTrees {
        assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
      }
    }*/

}
