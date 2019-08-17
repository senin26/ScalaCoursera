package patmat

import common._

/**
  * Assignment 4: Huffman coding
  *
  */
object Huffman {

  /**
    * A huffman code is represented by a binary tree.
    *
    * Every `Leaf` node of the tree represents one character of the alphabet that the tree can encode.
    * The weight of a `Leaf` is the frequency of appearance of the character.
    *
    * The branches of the huffman tree, the `Fork` nodes, represent a set containing all the characters
    * present in the leaves below it. The weight of a `Fork` node is the sum of the weights of these
    * leaves.
    */
  abstract class CodeTree {
    val weight: Int
  }

  case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree
  case class Leaf(char: Char, weight: Int) extends CodeTree


  // Part 1: Basics
  def weight(tree: CodeTree): Int = tree match {
    case Fork(left, right, _, _) => (weight(left) + weight(right))
    case Leaf(_, weight) => weight
  } // tree match ...

  def chars(tree: CodeTree): List[Char] = tree match {
    case Fork(left, right, _, _) => chars(left):::chars(right)
    case Leaf(char, _) => List(char)
  } // tree match ...

  def makeCodeTree(left: CodeTree, right: CodeTree) =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))



  // Part 2: Generating Huffman trees

  /**
    * In this assignment, we are working with lists of characters. This function allows
    * you to easily create a character list from a given string.
    */
  def string2Chars(str: String): List[Char] = str.toList

  /**
    * This function computes for each unique character in the list `chars` the number of
    * times it occurs. For example, the invocation
    *
    *   times(List('a', 'b', 'a'))
    *
    * should return the following (the order of the resulting list is not important):
    *
    *   List(('a', 2), ('b', 1))
    *
    * The type `List[(Char, Int)]` denotes a list of pairs, where each pair consists of a
    * character and an integer. Pairs can be constructed easily using parentheses:
    *
    *   val pair: (Char, Int) = ('c', 1)
    *
    * In order to access the two elements of a pair, you can use the accessors `_1` and `_2`:
    *
    *   val theChar = pair._1
    *   val theInt  = pair._2
    *
    * Another way to deconstruct a pair is using pattern matching:
    *
    *   pair match {
    *     case (theChar, theInt) =>
    *       println("character is: "+ theChar)
    *       println("integer is  : "+ theInt)
    *   }
    */
  def times(chars: List[Char]): List[(Char, Int)] = chars match {
    case List(x) => List(pair(List(x), x))
    case x::xs => filter(List(pair(chars, x)):::times(xs))
  }

  def times2(chars: List[Char]): List[(Char, Int)] = chars.groupBy(identity).map {
    case (char, list) => (char, list.size)
  }(collection.breakOut)

  def pair(charsIn: List[Char], c: Char): (Char, Int) = charsIn match {
    case List(x) => if (x==c) (x, 1) else (x, 0)
    case x::xs => if (x==c) (c, (pair(xs, c)_2).+(1)) else (c, (pair(xs, c)_2).+(0)) //pair(xs)_2
  }

  def filter(chars: List[(Char, Int)]): List[(Char, Int)] = {
    var filteredList = List(chars.head)
    var i = 1
    while(i < chars.length) {
      val next = chars(i)
      if (!containsCharOfPair(filteredList, next)) {
        filteredList = filteredList:::List(next)
      }
      i = i + 1
    }
    filteredList
  }

  def containsCharOfPair(list: List[(Char, Int)], pair: (Char, Int)): Boolean = {
    var j = 0
    var res = false
    while (j < list.length) {
      if (list(j)._1 == pair._1) {
        res = true
      }
      j = j + 1
    }
    res
  }


  /**
    * Returns a list of `Leaf` nodes for a given frequency table `freqs`.
    *
    * The returned list should be ordered by ascending weights (i.e. the
    * head of the list should have the smallest weight), where the weight
    * of a leaf is the frequency of the character.
    */
  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = freqs match {
    case List() => List()
    case x::xs => insert(Leaf(x._1, x._2), makeOrderedLeafList(xs))
  }

  def insert(leaf: Leaf, list: List[Leaf]): List[Leaf] = list match {
    case List() => List(leaf)
    case y::ys => if (y.weight >= leaf.weight) leaf::list else y::insert(leaf, ys)
  }

  /**
    * Checks whether the list `trees` contains only one single code tree.
    */
  def singleton(trees: List[CodeTree]): Boolean = {
    trees.length == 1
  }

  /**
    * The parameter `trees` of this function is a list of code trees ordered
    * by ascending weights.
    *
    * This function takes the first two elements of the list `trees` and combines
    * them into a single `Fork` node. This node is then added back into the
    * remaining elements of `trees` at a position such that the ordering by weights
    * is preserved.
    *
    * If `trees` is a list of less than two elements, that list should be returned
    * unchanged.
    */

  def forktwoTrees(treeL: CodeTree, treeR: CodeTree): CodeTree = treeL match {
    case Fork(left, right, chars, weight) => Fork(treeL, treeR,
      chars ::: getCharsOfCodeTree(treeR),
      weight + getWeightOfCodeTree(treeR))
    case Leaf(char, weight) => (Fork(treeR, treeL, List(char) ::: getCharsOfCodeTree(treeR), weight + getWeightOfCodeTree(treeR)))
  }

  def combine(trees: List[CodeTree]): List[CodeTree] = trees match {
    case x::xs => {
      sortTrees(List(forktwoTrees(x, xs.head)):::xs.tail)
    }
  }

  def sortTrees(treeList: List[CodeTree]): List[CodeTree] = {
    makeOrderedCodeTreeList(treeList)
  }

  // todo one can write an implementation of this methods just for List[CodeTree]
  def makeOrderedCodeTreeList(freqs: List[CodeTree]): List[CodeTree] = freqs match {
    case List() => List()
    case x::xs => insertCodeTree(x, makeOrderedCodeTreeList(xs))
  }

  def insertCodeTree(tree: CodeTree, list: List[CodeTree]): List[CodeTree] = list match {
    case List() => List(tree)
    case y::ys => if (y.weight >= tree.weight) tree::list else y::insertCodeTree(tree, ys)
  }

  // todo delete this, it's unused
  /*def combine(trees: List[CodeTree]): List[CodeTree] = trees match {
    case x :: xs => (x match {
      case Fork(left, right, chars, weight) =>
        List(Fork(x, xs.head,
          chars ::: getCharsOfCodeTree(xs.head),
          weight + getWeightOfCodeTree(xs.head)))
      case Leaf(char, weight) => List(Fork(x, xs.head, List(char) ::: getCharsOfCodeTree(xs.head), weight + getWeightOfCodeTree(xs.head)))
    }) ::: xs.tail
  }*/

  def getCharsOfCodeTree(trees: CodeTree): List[Char] = trees match {
    case Fork(left, right, chars, weight) => chars
    case Leaf(char, weight) => List(char)
  }

  def getWeightOfCodeTree(trees: CodeTree): Int = trees match {
    case Fork(left, right, chars, weight) => weight
    case Leaf(char, weight) => weight
  }

  /*  def listTest(list: List[(Leaf)]): List[Int] = list match {
      case List(x) => x match {
        case Leaf(chars, weight) => List(weight)
      }
      case x::xs => (x match {
        case Leaf(chars, weight) => List(weight)
      }) ::: listTest(xs)
    }*/

  /**
    * This function will be called in the following way:
    *
    *   until(singleton, combine)(trees)
    *
    * where `trees` is of type `List[CodeTree]`, `singleton` and `combine` refer to
    * the two functions defined above.
    *
    * In such an invocation, `until` should call the two functions until the list of
    * code trees contains only one single tree, and then return that singleton list.
    *
    * Hint: before writing the implementation,
    *  - start by defining the parameter types such that the above example invocation
    *    is valid. The parameter types of `until` should match the argument types of
    *    the example invocation. Also define the return type of the `until` function.
    *  - try to find sensible parameter names for `xxx`, `yyy` and `zzz`.
    */

  def until(f: List[CodeTree] => Boolean, y: List[CodeTree] => List[CodeTree])(trees: List[CodeTree]): List[CodeTree] = trees match {
    // todo looks like first 2 cases can be omitted
    case List() => Nil
    case List(x) => List(x)
    case x::xs => if (!f(trees))
      if(!xs.tail.isEmpty) y (y (x::List(xs.head)) :::
        until(f, y)(xs.tail)) else y(x::List(xs.head))
    else trees
    // if (!f(trees)) y(x::List(xs.head)) else trees // working snippet in one particular case
  }

  /**
    * This function creates a code tree which is optimal to encode the text `chars`.
    *
    * The parameter `chars` is an arbitrary text. This function extracts the character
    * frequencies from that text and creates a code tree based on them.
    */
  def createCodeTree(chars: List[Char]): CodeTree = {
    if (chars.length != 1) {
      if (!allCharsAreSame(chars)) {
        Fork(createCodeTree(getLeftCharList(sortCharList(chars))),
          createCodeTree(getRightCharList(sortCharList(chars))),
          getUniqueCharList(chars),
          chars.length)
      }
      else Leaf(chars(0), chars.length)
    }
    else Leaf(chars(0), 1)
  }

   def getLeftCharList(chars: List[Char]): List[Char] = {

    val leftList: List[Char] = chars.slice(0, chars.length/2)

    def addSimilarChars(charsIter: List[Char], accum: List[Char], char: Char): List[Char] = charsIter match {
      case List(x) => if (x==char) accum:::List(char) else accum
      case x::xs => if (x==char) addSimilarChars(xs, accum:::List(char), char) else accum
    }

    val leftList2 = addSimilarChars(chars.slice(chars.length/2, chars.length), leftList, leftList(leftList.length-1))
    if (leftList2.length == chars.length)
       getSingleCharsList(chars)
    else
       leftList2
  }

  def getRightCharList(chars: List[Char]): List[Char] = {
    val initIndex = getLeftCharList(chars).length
    if (initIndex == chars.length)
      getRepetitiveCharsList(chars)
    else
      chars.slice(initIndex, chars.length)
  }

  def getSingleCharsList(chars: List[Char]): List[Char] = {
    def iter(charsIter: List[Char], accum: List[Char]): List[Char] = charsIter match {
      case List(x) => if (!accum.contains(x) && getAmountCharInList(chars, x)==1) accum:::List(x) else accum
      case x::xs => if (!accum.contains(x) && getAmountCharInList(chars, x)==1) iter(xs, accum:::List(x)) else iter(xs, accum)
    }
    iter(chars, List())
  }

  def getRepetitiveCharsList(chars: List[Char]): List[Char] = {
    def iter(charsIter: List[Char], accum: List[Char]): List[Char] = charsIter match {
      case List(x) => accum
      case x::xs => if (!accum.contains(x) && xs.contains(x)) iter(xs, accum:::List(x)) else iter(xs, accum)
    }
    iter(chars, List())
  }

  def getUniqueCharList(chars: List[Char]): List[Char] = {
    def iter(charsIter: List[Char], accum: List[Char]): List[Char] = charsIter match {
      case List(x) => if (!accum.contains(x)) accum:::List(x) else accum
      case x::xs => if (!accum.contains(xs.head)) iter(xs, accum:::List(xs.head))
      else iter(xs, accum)
    }
    iter(chars, List(chars(0)))
  }
  //todo remove the following 2 def's since they're not done in a functional way
  /*def getLeftCharList(list: List[Char]): List[Char] = {
    var leftList: List[Char] = List()
    var i = 0
    leftList = list.slice(0, list.length/2)
    while (leftList(leftList.length-1) == list(leftList.length)) {
      leftList = leftList:::List(list(leftList.length))
    }
    leftList
  }*/
 /* def getRightCharList(list: List[Char]): List[Char] = {
    var rightList: List[Char] = List()
    var j = getLeftCharList(list).length
    while (j < list.length) {
      rightList = rightList:::List(list(j))
      j = j + 1
    }
    rightList
  }*/

  def sortCharList(chars: List[Char]): List[Char] = chars match {
    case List() => List()
    case x::xs => insertChar(x, sortCharList(xs))
  }

  def insertChar(char: Char, list: List[Char]): List[Char] = list match {
    case List() => List(char)
    case y::ys => if (y >= char) char::list else y::insertChar(char, ys)
  }

  def allCharsAreSame(chars: List[Char]): Boolean = {
    def iter(a: Int): Boolean = {
      if (a>=1) chars(a) == chars(a-1) && iter(a-1)
      else true
    }
    iter(chars.length-1)
  }

    def getAmountCharInList(list: List[Char], char: Char): Int = {
      def iter(list: List[Char], char: Char, accum: Int): Int = list match {
        case List(x) => if (x==char) accum+1 else accum
        case x::xs => {
          if (x==char) iter(xs, char, accum+1) else iter(xs, char, accum)
        }
      }
      iter(list, char, 0)
    }

  // Part 3: Decoding

  type Bit = Int

  /**
    * This function decodes the bit sequence `bits` using the code tree `tree` and returns
    * the resulting list of characters.
    */
  def decode(tree: CodeTree, bits: List[Bit]): List[Char] = {
    def iter(treeIter: CodeTree, bits: List[Bit], accum: List[Char]): List[Char] = bits match {
      case List() => accum
      case List(x) => treeIter match {
        case Fork(left, right, chars, weight) =>
          if (x == 0) iter(left, List(x), accum) else if (x == 1) iter(right, List(x), accum) else throw new Error("Illegal bit sequence")
        case Leaf(char, weight) => iter(tree, List(), accum ::: List(char))
      }
      case List(x, y) => treeIter match {
        case Fork(left, right, chars, weight) => if (isTreeSimple(tree, treeIter)) {
          if (x == 0) accum:::List(left.asInstanceOf[Leaf].char, right.asInstanceOf[Leaf].char)
          else if (x == 1) accum:::List(right.asInstanceOf[Leaf].char, left.asInstanceOf[Leaf].char)
          else throw new Error("Illegal bit sequence")
        }
          else
          {
            if (x == 0) iter(left, List(y), accum) else if (x == 1) iter(right, List(y), accum) else throw new Error("Illegal bit sequence")
          }
        case Leaf(char, weight) => iter(tree, List(x, y), accum ::: List(char))
      }
      case x :: xs => treeIter match {
        /*case Fork(Leaf(charL, weightL), Leaf(charR, weightR), List(charL, charR), weightL+weightR) => if (x==0) iter(Leaf(charL, weightL), x::xs, accum)
                        else if (x==1) iter(Leaf(charR, weightR), x::xs, accum) else throw new Error("Illegal bit sequence")*/
        case Fork(left, right, chars, weight) =>
          if (x == 0) iter(left, xs, accum) else if (x == 1) iter(right, xs, accum) else throw new Error("Illegal bit sequence")

        case Leaf(char, weight) => iter(tree, x :: xs, accum ::: List(char))
      }
    }

    iter(tree, bits, List())
  }

  def isTreeSimple(baseTree: CodeTree, tree: CodeTree): Boolean = tree match {
    case Fork(left, right, chars, weight) => tree == baseTree && left.isInstanceOf[Leaf] && right.isInstanceOf[Leaf]
  }

  /**
    * A Huffman coding tree for the French language.
    * Generated from the data given at
    *   http://fr.wikipedia.org/wiki/Fr%C3%A9quence_d%27apparition_des_lettres_en_fran%C3%A7ais
    */
  val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s',121895),Fork(Leaf('d',56269),Fork(Fork(Fork(Leaf('x',5928),Leaf('j',8351),List('x','j'),14279),Leaf('f',16351),List('x','j','f'),30630),Fork(Fork(Fork(Fork(Leaf('z',2093),Fork(Leaf('k',745),Leaf('w',1747),List('k','w'),2492),List('z','k','w'),4585),Leaf('y',4725),List('z','k','w','y'),9310),Leaf('h',11298),List('z','k','w','y','h'),20608),Leaf('q',20889),List('z','k','w','y','h','q'),41497),List('x','j','f','z','k','w','y','h','q'),72127),List('d','x','j','f','z','k','w','y','h','q'),128396),List('s','d','x','j','f','z','k','w','y','h','q'),250291),Fork(Fork(Leaf('o',82762),Leaf('l',83668),List('o','l'),166430),Fork(Fork(Leaf('m',45521),Leaf('p',46335),List('m','p'),91856),Leaf('u',96785),List('m','p','u'),188641),List('o','l','m','p','u'),355071),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u'),605362),Fork(Fork(Fork(Leaf('r',100500),Fork(Leaf('c',50003),Fork(Leaf('v',24975),Fork(Leaf('g',13288),Leaf('b',13822),List('g','b'),27110),List('v','g','b'),52085),List('c','v','g','b'),102088),List('r','c','v','g','b'),202588),Fork(Leaf('n',108812),Leaf('t',111103),List('n','t'),219915),List('r','c','v','g','b','n','t'),422503),Fork(Leaf('e',225947),Fork(Leaf('i',115465),Leaf('a',117110),List('i','a'),232575),List('e','i','a'),458522),List('r','c','v','g','b','n','t','e','i','a'),881025),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u','r','c','v','g','b','n','t','e','i','a'),1486387)

  /**
    * What does the secret message say? Can you decode it?
    * For the decoding use the `frenchCode' Huffman tree defined above.
    */
  val secret: List[Bit] = List(0,0,1,1,1,0,1,0,1,1,1,0,0,1,1,0,1,0,0,1,1,0,1,0,1,1,0,0,1,1,1,1,1,0,1,0,1,1,0,0,0,0,1,0,1,1,1,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,1)

  /**
    * Write a function that returns the decoded secret
    */
  def decodedSecret: List[Char] = decode(frenchCode, secret)


  // Part 4a: Encoding using Huffman tree

  /**
    * This function encodes `text` using the code tree `tree`
    * into a sequence of bits.
    */
  def encode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    def iter(treeIter: CodeTree, textIter: List[Char], accum: List[Bit]): List[Bit] = textIter match {
      case List(x) => treeIter match {
        case Fork(left, right, chars, weight) => if (goToTheLeft(treeIter, x)) iter(left, List(x), accum:::List(0))
                                                 else iter(right, List(x), accum:::List(1))
        case Leaf(char, weight) => accum
      }
      case x::xs => treeIter match {
        case Fork(left, right, chars, weight) => if (goToTheLeft(treeIter, x)) iter(left, x::xs, accum:::List(0))
                                                 else iter(right, x::xs, accum:::List(1))
        case Leaf(char, weight) => iter(tree, xs, accum)
      }
    }
    iter(tree, text, List())
  }

  /*This is a service function to select whether one should go to the left subtree or to the right*/
  def goToTheLeft(tree: CodeTree, char: Char): Boolean = tree match {
    case Fork(left, right, chars, weight) => left match {
      case Fork(leftIn, rightIn, charsIn, weightIn) => charsIn.contains(char)
      case Leaf(charIn, weight) => charIn==char
    }
  }

  // Part 4b: Encoding using code table

  type CodeTable = List[(Char, List[Bit])]

  /**
    * This function returns the bit sequence that represents the character `char` in
    * the code table `table`.
    */
  def codeBits(table: CodeTable)(char: Char): List[Bit] = {

    def iter(table: CodeTable, char: Char): List[Bit] = table match {
      case List(x) => if (x._1 == char) x._2 else List()
      case x::xs => if (x._1 == char) x._2 else iter(xs, char)
    }
    iter(table, char)

  }

  /**
    * Given a code tree, create a code table which contains, for every character in the
    * code tree, the sequence of bits representing that character.
    *
    * Hint: think of a recursive solution: every sub-tree of the code tree `tree` is itself
    * a valid code tree that can be represented as a code table. Using the code tables of the
    * sub-trees, think of how to build the code table for the entire tree.
    */
  def convert(tree: CodeTree): CodeTable = {

    def iter(treeIter: CodeTree, chars: List[Char], accum: CodeTable, accumBits: List[Bit]): CodeTable = chars match {
      case List(x) => treeIter match {
        case Fork(left, right, chars, weight) => if (goToTheLeft(treeIter, x)) iter(left, List(x), accum, accumBits:::List(0))
                                                 else iter(right, List(x), accum, accumBits:::List(1))
        case Leaf(char, weight) => accum:::List((char, accumBits))
      }
      case x::xs => treeIter match {
        case Fork(left, right, chars, weight) => if (goToTheLeft(treeIter, x)) iter(left, x::xs, accum, accumBits:::List(0))
                                                 else iter(right, x::xs, accum, accumBits:::List(1))
        case Leaf(char, weight) => iter(tree, xs, accum:::List((char, accumBits)), List())
      }
    }

    def invoke(tree: CodeTree): CodeTable = tree match {
      case Fork(left, right, chars, weight) => iter(tree, chars, List(), List())
      case Leaf(char, weight) => iter(tree, List(char), List(), List())
    }

    invoke(tree)

  }

  /**
    * This function takes two code tables and merges them into one. Depending on how you
    * use it in the `convert` method above, this merge method might also do some transformations
    * on the two parameter code tables.
    */
  def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = {
    val list1 = getCharsFromCodeTable(a, List())
    val list2 = getCharsFromCodeTable(b, List())
    val list = list1:::list2
    val codeTree = createCodeTree(list)
    convert(codeTree)
  }

  /*This is the service function that outputs the List[Char] of the chars from the CodeTable*/
  def getCharsFromCodeTable(codeTable: CodeTable, accum: List[Char]): List[Char] = codeTable match {
    case List(x) => accum:::List(x._1)
    case x::xs => getCharsFromCodeTable(xs, accum:::List(x._1))
  }

  /**
    * This function encodes `text` according to the code tree `tree`.
    *
    * To speed up the encoding process, it first converts the code tree to a code table
    * and then uses it to perform the actual encoding.
    */
  def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    val codeTable = convert(tree)

    def iter(codeTable: CodeTable, text: List[Char], accum: List[Bit]): List[Bit] = text match {
      case List(x) => accum:::codeBits(codeTable)(x)
      case x::xs => iter(codeTable, xs, accum:::codeBits(codeTable)(x))
    }

    iter(codeTable, text, List())

  }
}