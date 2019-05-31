package objsets

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class DifferentTests extends FunSuite {
  trait TestSets {
    val set1 = new Empty
    val set2 = set1.incl(new Tweet("a", "d tweet", 21))
    // val set2 = set1.incl(new Tweet("a", "a body", 20))
    val set3 = set2.incl(new Tweet("b", "z absltly fucking awesome tweet", 21))
    val c = new Tweet("a", "t fucking tweet", 25)
    val d = new Tweet("d", "b tweet", 9)
    val e = new Tweet("d", "a twt", 15)
    val set4c = set3.incl(c)
    val set4d = set3.incl(d)
    val set5 = set4c.incl(d)
    val set6 = set5.incl(e)
  }

  test("compare 2 strings") {
    new TestSets {
      assert("absltly fucking awesome tweet">"ya tweet")
    }
  }

  test("compare 2 strings again") {
    new TestSets {
      assert("zabsltly fucking awesome tweet"<"ya tweet")
    }
  }

/*

  def asSet(tweets: TweetSet): Set[Tweet] = {
    var res = Set[Tweet]()
    tweets.foreach(res += _)
    res
  }

  def size(set: TweetSet): Int = asSet(set).size

  test("filter: on empty set") {
    new TestSets {
      assert(size(set1.filter(tw => tw.user == "a")) === 0)
    }
  }

  test("filter: a on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.user == "a")) === 1)
    }
  }

  test("filter: 20 on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.retweets == 20)) === 2)
    }
  }

  test("union: set4c and set4d") {
    new TestSets {
      assert(size(set4c.union(set4d)) === 4)
    }
  }

  test("union: with empty set (1)") {
    new TestSets {
      assert(size(set5.union(set1)) === 4)
    }
  }

  test("union: with empty set (2)") {
    new TestSets {
      assert(size(set1.union(set5)) === 4)
    }
  }

  test("find most retweeted tweet") {
    new TestSets {
      val setTemp = set5
      val trends = set5.mostRetweeted
      //assert(!trends.isEmpty)
      //assert(trends.head.user == "a" )
      assert(trends.user == "a")
      assert(trends.retweets == 25)
    }
  }

  test("descending: set5") {
    new TestSets {
      val trends = set5.descendingByRetweet
      assert(!trends.isEmpty)
      assert(trends.head.user == "a" || trends.head.user == "b")
    }
  }*/

  }