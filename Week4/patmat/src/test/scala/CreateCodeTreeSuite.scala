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

  test("decode test 1") {
    new TestTrees {
      val bitsList = List(1,0,0,0,1)
      val text = "texxt"   // val text = "This is a fucking important message"
      val charList: List[Char]  = sortCharList(text.toList)

      val leaf1 = Leaf('e', 1)
      val leaf2 = Leaf('t', 2)
      val leaf3 = Leaf('x', 2)
      val fork1 = Fork(leaf1, leaf2, List(leaf1.char, leaf2.char), leaf1.weight+leaf2.weight)
      val fork2 = Fork(fork1, leaf3, List(leaf1.char, leaf2.char, leaf3.char), fork1.weight+leaf3.weight)

      assert(decode(fork2, bitsList) === "xet".toList)
    }
  }

  test("decode test 2") {
    new TestTrees {
      val bitsList = List(1,1,0,0,0,1,0,1)
      val text = "texxt"   // val text = "This is a fucking important message"
      val charList: List[Char]  = sortCharList(text.toList)

      val leaf1 = Leaf('e', 1)
      val leaf2 = Leaf('t', 2)
      val leaf3 = Leaf('x', 2)
      val fork1 = Fork(leaf1, leaf2, List(leaf1.char, leaf2.char), leaf1.weight+leaf2.weight)
      val fork2 = Fork(fork1, leaf3, List(leaf1.char, leaf2.char, leaf3.char), fork1.weight+leaf3.weight)

      assert(decode(fork2, bitsList) === "xxett".toList)
    }
  }

  test("secret message decode test 2") {
    new TestTrees {
      val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s',121895),Fork(Leaf('d',56269),Fork(Fork(Fork(Leaf('x',5928),Leaf('j',8351),List('x','j'),14279),Leaf('f',16351),List('x','j','f'),30630),Fork(Fork(Fork(Fork(Leaf('z',2093),Fork(Leaf('k',745),Leaf('w',1747),List('k','w'),2492),List('z','k','w'),4585),Leaf('y',4725),List('z','k','w','y'),9310),Leaf('h',11298),List('z','k','w','y','h'),20608),Leaf('q',20889),List('z','k','w','y','h','q'),41497),List('x','j','f','z','k','w','y','h','q'),72127),List('d','x','j','f','z','k','w','y','h','q'),128396),List('s','d','x','j','f','z','k','w','y','h','q'),250291),Fork(Fork(Leaf('o',82762),Leaf('l',83668),List('o','l'),166430),Fork(Fork(Leaf('m',45521),Leaf('p',46335),List('m','p'),91856),Leaf('u',96785),List('m','p','u'),188641),List('o','l','m','p','u'),355071),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u'),605362),Fork(Fork(Fork(Leaf('r',100500),Fork(Leaf('c',50003),Fork(Leaf('v',24975),Fork(Leaf('g',13288),Leaf('b',13822),List('g','b'),27110),List('v','g','b'),52085),List('c','v','g','b'),102088),List('r','c','v','g','b'),202588),Fork(Leaf('n',108812),Leaf('t',111103),List('n','t'),219915),List('r','c','v','g','b','n','t'),422503),Fork(Leaf('e',225947),Fork(Leaf('i',115465),Leaf('a',117110),List('i','a'),232575),List('e','i','a'),458522),List('r','c','v','g','b','n','t','e','i','a'),881025),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u','r','c','v','g','b','n','t','e','i','a'),1486387)

      val secret: List[Bit] = List(0,0,1,1,1,0,1,0,1,1,1,0,0,1,1,0,1,0,0,1,1,0,1,0,1,1,0,0,1,1,1,1,1,0,1,0,1,1,0,0,0,0,1,0,1,1,1,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,1)

    /*  val bitsList = List(1,1,0,0,0,1,0,1)
      val text = "texxt"   // val text = "This is a fucking important message"
      val charList: List[Char]  = sortCharList(text.toList)

      val leaf1 = Leaf('e', 1)
      val leaf2 = Leaf('t', 2)
      val leaf3 = Leaf('x', 2)
      val fork1 = Fork(leaf1, leaf2, List(leaf1.char, leaf2.char), leaf1.weight+leaf2.weight)
      val fork2 = Fork(fork1, leaf3, List(leaf1.char, leaf2.char, leaf3.char), fork1.weight+leaf3.weight)*/

      assert(decode(frenchCode, secret) === "smth".toList)
    }
  }

  test("message encode test 1") {
    new TestTrees {
        val bitsList = List(1,0,0,0,1)
        val text = "xet"

        val leaf1 = Leaf('e', 1)
        val leaf2 = Leaf('t', 2)
        val leaf3 = Leaf('x', 2)
        val fork1 = Fork(leaf1, leaf2, List(leaf1.char, leaf2.char), leaf1.weight+leaf2.weight)
        val tree = Fork(fork1, leaf3, List(leaf1.char, leaf2.char, leaf3.char), fork1.weight+leaf3.weight)

      assert(encode(tree)(text.toList) === bitsList)
    }
  }

  test("codebits test") {
    new TestTrees {
      val codeTable: CodeTable = List(('e', List(0,0)),('t', List(0,1)),('x', List(1)))
      assert(codeBits(codeTable)('u') === List())
    }
  }

  test("convert tree to codeTable test") {
    new TestTrees {
      val codeTable = List(('e', List(0,0)), ('t', List(0,1)), ('x', List(1)))
      val leaf1 = Leaf('e', 1)
      val leaf2 = Leaf('t', 2)
      val leaf3 = Leaf('x', 2)
      val fork1 = Fork(leaf1, leaf2, List(leaf1.char, leaf2.char), leaf1.weight+leaf2.weight)
      val tree = Fork(fork1, leaf3, List(leaf1.char, leaf2.char, leaf3.char), fork1.weight+leaf3.weight)

      assert(convert(tree) === codeTable)
      //assert(convert(leaf1) === List(('e', List(0))))
    }
  }

  test("quickEncode test") {
    new TestTrees {

      val code = List(1,0,0,0,1)
      val text = "xet"

      val bitsList = List(('e', List(0,0)), ('t', List(0,1)), ('x', List(1)))
      val leaf1 = Leaf('e', 1)
      val leaf2 = Leaf('t', 2)
      val leaf3 = Leaf('x', 2)
      val fork1 = Fork(leaf1, leaf2, List(leaf1.char, leaf2.char), leaf1.weight+leaf2.weight)
      val tree = Fork(fork1, leaf3, List(leaf1.char, leaf2.char, leaf3.char), fork1.weight+leaf3.weight)

      assert(quickEncode(tree)(text.toList) === code)
    }
  }

  test("getCharsFromCodeTable test") {
    new TestTrees {

      val code = List(1,0,0,0,1)
      val text = "xet"

      val bitsList = List(('e', List(0,0)), ('t', List(0,1)), ('x', List(1)))
      val leaf1 = Leaf('e', 1)
      val leaf2 = Leaf('t', 2)
      val leaf3 = Leaf('x', 2)
      val fork1 = Fork(leaf1, leaf2, List(leaf1.char, leaf2.char), leaf1.weight+leaf2.weight)
      val tree = Fork(fork1, leaf3, List(leaf1.char, leaf2.char, leaf3.char), fork1.weight+leaf3.weight)

      assert(getCharsFromCodeTable(bitsList, List()) === "etx".toList)
    }
  }

  test("mergeCodeTables test") {
    new TestTrees {
      val bitsList1 = List(('e', List(0,0)), ('t', List(0,1)), ('x', List(1)))
      val bitsList2 = List(('e', List(0,0)), ('r', List(0,1)), ('t', List(1)))
      assert(mergeCodeTables(bitsList1, bitsList2) === bitsList2)
    }
  }


  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("1") {
    new TestTrees {
      assert(encode(t1)("ab".toList) === List(0,1))
    }
  }

  test("2") {
    new TestTrees {
      assert(decode(t1, List(0,1)) === "ab".toList)
    }
  }

}
