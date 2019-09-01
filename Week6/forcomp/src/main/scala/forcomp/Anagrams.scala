package forcomp


object Anagrams {

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /** `Occurrences` is a `List` of pairs of characters and positive integers saying
   *  how often the character appears.
   *  This list is sorted alphabetically w.r.t. to the character in each pair.
   *  All characters in the occurrence list are lowercase.
   *
   *  Any list of pairs of lowercase characters and their frequency which is not sorted
   *  is **not** an occurrence list.
   *
   *  Note: If the frequency of some character is zero, then that character should not be
   *  in the list.
   */
  type Occurrences = List[(Char, Int)]

  /** The dictionary is simply a sequence of words.
   *  It is predefined and obtained as a sequence using the utility method `loadDictionary`.
   */
  val dictionary: List[Word] = loadDictionary

  /** Converts the word into its character occurrence list.
   *
   *  Note: the uppercase and lowercase version of the character are treated as the
   *  same character, and are represented as a lowercase character in the occurrence list.
   *
   *  Note: you must use `groupBy` to implement this method!
   */
  def wordOccurrences(w: Word): Occurrences = {
    w.toLowerCase.groupBy(identity).map{
      case (char, times) => (char, times.length)
    }.toList.sortBy(_._1)
  }

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences = wordOccurrences(s.foldLeft("")(_ + _))

  /** The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
   *  the words that have that occurrence count.
   *  This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
   *
   *  For example, the word "eat" has the following character occurrence list:
   *
   *     `List(('a', 1), ('e', 1), ('t', 1))`
   *
   *  Incidentally, so do the words "ate" and "tea".
   *
   *  This means that the `dictionaryByOccurrences` map will contain an entry:
   *
   *    List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
   *
   */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = dictionary.groupBy(w => wordOccurrences(w))

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] = dictionaryByOccurrences.getOrElse(wordOccurrences(word), List())

  /** Returns the list of all subsets of the occurrence list.
   *  This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
   *  is a subset of `List(('k', 1), ('o', 1))`.
   *  It also include the empty subset `List()`.
   *
   *  Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
   *
   *    List(
   *      List(),
   *      List(('a', 1)),
   *      List(('a', 2)),
   *      List(('b', 1)),
   *      List(('a', 1), ('b', 1)),
   *      List(('a', 2), ('b', 1)),
   *      List(('b', 2)),
   *      List(('a', 1), ('b', 2)),
   *      List(('a', 2), ('b', 2))
   *    )
   *
   *  Note that the order of the occurrence list subsets does not matter -- the subsets
   *  in the example above could have been displayed in some other order.
   */
  //def combinations(occurrences: Occurrences): List[Occurrences] =  ???
  def combinations(occurrences: Occurrences): List[Occurrences] = {

   /* for {
      split <- (1 to occurrences.length).toList
      elem <- 1 to occurrences(split)._2
      rest <- combinations(occurrences drop split)
    } yield (occurrences(split), elem) :: rest*/

   /* for {
      //split <- (1 to occurrences.length).toList
      tuple <- occurrences take 1
      //elem <- 1 to tuple._2
      rest <- combinations(occurrences drop 1)
    } yield (tuple) :: rest*/

    def subOccurences(list: (Char, Int)): List[(Char, Int)] = list match {
      case (_: Char, int: Int) if int == 0 => List.empty
      case (char: Char, int: Int) if int > 0 => List((char, int)) ::: subOccurences((char, int-1))
    }

    (0 to occurrences.length-1)
    .flatMap(split =>
    List(subOccurences(occurrences(split)))
    /*.flatMap(subs =>
    combinations(occurrences drop (split+1)).map(rest => rest ::: subs))*/
    ).toList


    /*val word = occurrences.foldLeft("")((x,y) => {
      var res = ""
      for (i <- 0 until y._2)
        res += y._1
      x+res
    })

    var listWords: List[Word] = List()
    for(i <- 0 to word.length) {
      for(j <- i to word.length) {
        listWords = listWords:::List(word.substring(i,j))
      }
    }

    listWords = listWords.toSet.toList.sortWith((w1, w2) => {
      if (!w1.isEmpty && !w2.isEmpty) w1.charAt(0).compareTo(w2.charAt(0))<0
      else if (w1.isEmpty) true
      else false
    })

    var listWordOccurences: List[Occurrences] = List()
    for(k <- 0 until listWords.length) {
      listWordOccurences = listWordOccurences:::List(wordOccurrences(listWords(k)))
    }
    listWordOccurences*/
  }

  /** Subtracts occurrence list `y` from occurrence list `x`.
   *
   *  The precondition is that the occurrence list `y` is a subset of
   *  the occurrence list `x` -- any character appearing in `y` must
   *  appear in `x`, and its frequency in `y` must be smaller or equal
   *  than its frequency in `x`.
   *
   *  Note: the resulting value is an occurrence - meaning it is sorted
   *  and has no zero-entries.
   */
  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    //x.filter(z => !y.map(z => z._1).contains(z._1))
    //val yMap = y.toMap
    x.map(z => if (y.map(p => p._1).contains(z._1) && y.toMap.getOrElse(z._1, 0)<=z._2)
      (z._1, z._2-y.toMap.getOrElse(z._1, 0))
        else z)
      .filter(z => z._2!=0)
  }

  /** Returns a list of all anagram sentences of the given sentence.
   *
   *  An anagram of a sentence is formed by taking the occurrences of all the characters of
   *  all the words in the sentence, and producing all possible combinations of words with those characters,
   *  such that the words have to be from the dictionary.
   *
   *  The number of words in the sentence and its anagrams does not have to correspond.
   *  For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
   *
   *  Also, two sentences with the same words but in a different order are considered two different anagrams.
   *  For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
   *  `List("I", "love", "you")`.
   *
   *  Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
   *
   *    List(
   *      List(en, as, my),
   *      List(en, my, as),
   *      List(man, yes),
   *      List(men, say),
   *      List(as, en, my),
   *      List(as, my, en),
   *      List(sane, my),
   *      List(Sean, my),
   *      List(my, en, as),
   *      List(my, as, en),
   *      List(my, sane),
   *      List(my, Sean),
   *      List(say, men),
   *      List(yes, man)
   *    )
   *
   *  The different sentences do not have to be output in the order shown above - any order is fine as long as
   *  all the anagrams are there. Every returned word has to exist in the dictionary.
   *
   *  Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
   *  so it has to be returned in this list.
   *
   *  Note: There is only one anagram of an empty sentence.
   */
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {

    val occurrences = sentenceOccurrences(sentence)
    val combs = combinations(occurrences)
    //lazy val dictionaryMap: Map[Occurrences, List[Word]] = dictionaryByOccurrences

    var listSentences: List[Sentence] = Nil
    var curWord: Word = ""
    var curAnagrams: List[Word] = List()
    var curSentence: Sentence = List()
    var curOccurence: Occurrences = Nil
    var curWordOccurence: Occurrences = List()
    var wordIsFit = false // mark whether this word could be the 1st word in the output

    for (i <- 0 until dictionary.length) {
      curWord = dictionary(i)
      curWordOccurence = wordOccurrences(curWord)
      curOccurence = occurrences
      if (isSubOccurence(curOccurence, curWordOccurence)) {
        curOccurence = subtract(curOccurence, curWordOccurence)
        curSentence = curSentence:::List(curWord)
        for (k <- 0 until dictionary.length if (/*k!= i &&*/ !curOccurence.isEmpty)) {
          curWord = dictionary(k)
          curWordOccurence = wordOccurrences(curWord)
          if (isSubOccurence(curOccurence, curWordOccurence)) {
            curOccurence = subtract(curOccurence, curWordOccurence)
            curSentence = curSentence:::List(curWord)
          }
          if (curOccurence.isEmpty) {
            listSentences = listSentences:::List(curSentence)
            wordIsFit = true
          }
        }
      }
      if (wordIsFit) {
        curWord = dictionary(i)
        curAnagrams = wordAnagrams(curWord)
        for(j <- 0 until {if (!curAnagrams.isEmpty) curAnagrams.length else 1}) {
          var list: Sentence  = if (!curSentence.tail.isEmpty) List(curAnagrams(j)):::curSentence.tail else List(curAnagrams(j))
          listSentences = listSentences:::List(list)
        }
        wordIsFit = false
      }
      curSentence = List()
    }
    listSentences
  }

  def isSubOccurence(sup: Occurrences, sub: Occurrences): Boolean = {
    var res = true
    val supMap = sup.toMap
    val subCharKeys = sub.map(x => x._1)
    for(i <- 0 until subCharKeys.length) {
      if (supMap.getOrElse(subCharKeys(i), 0) == 0) res = false
    }
    res
  }
}
