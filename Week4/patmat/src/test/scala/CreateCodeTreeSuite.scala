package patmat

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class CreateCodeTreeSuite extends FunSuite {
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


  test("get sorted list") {
    new TestTrees {
      val text = "texxt"   // val text = "This is a fucking important message"
      val charList: List[Char]  = text.toList
      assert(sortCharList(charList) === "ettxx".toList)
    }
  }

  test("get left char list") {
    new TestTrees {
      val text = "etttt"   // val text = "This is a fucking important message"
      val charList: List[Char]  = text.toList
      assert(getLeftCharList(sortCharList(charList)) === "e".toList)
    }
  }

  test("get right char list") {
    new TestTrees {
      val text = "texxt"   // val text = "This is a fucking important message"
      val charList: List[Char]  = text.toList
      assert(getRightCharList(sortCharList(charList)) === "xx".toList)
    }
  }

 /* test("all chars are same") {
    new TestTrees {
      val text = "tttt"   // val text = "This is a fucking important message"
      val charList: List[Char]  = text.toList
      assert(allCharsAreSame(charList) === true)
    }
  }*/

/*  test("sort single char") { //this test is ok
    new TestTrees {
      val text = ""   // val text = "This is a fucking important message"
      val charList: List[Char]  = text.toList
      assert(sortCharList(charList) === "".toList)
    }
  }*/

  test("get unique chars list") {
    new TestTrees {
      val text = "texxtzzuiuz"   // val text = "This is a fucking important message"  texxtzzuiuz
      val charList: List[Char]  = text.toList
      assert(getUniqueCharList(charList) === "texzui".toList)
    }
  }

  test("getSingleCharsList test") {
    new TestTrees {
      val text = "zatytbtyzx"   // val text = "This is a fucking important message"
      val charList: List[Char]  = text.toList
      assert(getSingleCharsList(charList) === "abx".toList)
    }
  }

  test("getRepetitiveCharsList test") {
    new TestTrees {
      val text = "ztyttyzx"   // val text = "This is a fucking important message"
      val charList: List[Char]  = text.toList
      assert(getRepetitiveCharsList(charList) === "zty".toList)
    }
  }

  test("getAmountCharInList test") {
    new TestTrees {
      val text = "zytyttyzyx"   // val text = "This is a fucking important message"
      val charList: List[Char]  = text.toList
      assert(getAmountCharInList(charList, 'a') === 0)
    }
  }

  //getSingeCharsList
  // getRepetitiveCharsList


  test("create code tree ") {
    new TestTrees {
      val text = "texxt"   // val text = "This is a fucking important message"
      val charList: List[Char]  = sortCharList(text.toList)
      // CodeTree #1
      val leaf1 = Leaf('e', 1)
      val leaf2 = Leaf('t', 2)
      val leaf3 = Leaf('x', 2)
      val fork1 = Fork(leaf1, leaf2, List(leaf1.char, leaf2.char), leaf1.weight+leaf2.weight)
      val fork2 = Fork(fork1, leaf3, List(leaf1.char, leaf2.char, leaf3.char), fork1.weight+leaf3.weight)
      assert(createCodeTree(charList) === fork2)
    }
  }

  test("create code tree in case of 'etttt' input list ") {
    new TestTrees {
      val text = "tttte"   // val text = "This is a fucking important message"
      val charList: List[Char]  = text.toList
      // CodeTree #1
      val leaf1 = Leaf('e', 1)
      val leaf2 = Leaf('t', 4)
      val fork1 = Fork(leaf1, leaf2, List(leaf2.char, leaf1.char), leaf1.weight+leaf2.weight)
      assert(createCodeTree(charList) === fork1)
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
