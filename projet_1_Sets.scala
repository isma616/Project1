package project1

object Sets {
  /**
   * This type alias defines how sets are represented.
   */
  type Set = Int => Boolean

  /**
   * This function tests whether a set contains a given element.
   */
  def contains(s: Set, elem: Int): Boolean = s(elem)

  /**
   * This function creates a set with only one element
   */
  def set(elem: Int): Set = a => elem == a

  /**
   * Returns the union of two given sets
   */
  def union(s: Set, t: Set): Set = a => s(a) || t(a)

  /**
   * Returns the intersection of two given sets
   */
  def intersect(s: Set, t: Set): Set = a => s(a) && t(a)

  /**
   * Returns the difference between two given sets
   */
  def diff(s: Set, t: Set): Set = a => s(a) && !t(a)

  /**
   * Returns a new set that contains all elements of s that satisfy condition p
   */
  def filter(s: Set, p: Int => Boolean): Set = a => s(a) && p(a)

  /**
   * Returns true if condition p is satisfied for every element of s
   * (Note: this implementation only tests elements within [-1000, 1000]
   */
  def forall(s: Set, p: Int => Boolean): Boolean = {
    val f = filter(s, a => !p(a))
    def iter(a: Int): Boolean = {
      if (f(a)) false
      else if (a > 1000) true
      else iter(a + 1)
    }
    iter(-1000)
  }

  /**
   * Returns true if there exists one element of s that satisfies condition p
   * (Note: this implementation only tests elements within [-1000, 1000]
   */
  def exists(s: Set, p: Int => Boolean): Boolean = !forall(s, a => !p(a))

  /**
   * Construct a set from s such that inputs are transformed with function f
   */
  def map(s: Set, f: Int => Int): Set = a => exists(s, b => f(b) == a)

  /**
   * This function displays the contents of a set in the
   * Test Pad (Scala plugin for Eclipse).
   */
  def toString(s: Set): String = {
    val xs = for (i <- -1000 to 1000 if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  /**
   * This function prints the contents of a set on the console.
   */
  def printSet(s: Set) {
    println(toString(s))
  }

  //////////////////////////////////////////////////
  //// Seconde partie : ensemble orientes-objet ////
  //////////////////////////////////////////////////

  abstract class IntSet {
    def incl(x: Int): IntSet
    def contains(x: Int): Boolean

    def intersect(that: IntSet): IntSet = {
      filter(a => that contains(a))
    }

    def filter(p: Int => Boolean): IntSet = {
      filter0(p, new Empty)
    }
    def filter0(p: Int => Boolean, accu: IntSet): IntSet

    override def toString: String = {
      val xs = for (i <- -1000 to 1000 if contains(i)) yield i
      xs.mkString("{", ",", "}")
    }
  }

  class Empty extends IntSet {
    def contains(x: Int): Boolean = false
    def incl(x: Int): IntSet = new NonEmpty(x, new Empty, new Empty)
    def filter0(p: Int => Boolean, accu: IntSet): IntSet = accu
  }

  class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
    def contains(x: Int): Boolean =
      if (x < elem) left.contains(x)
      else if (x > elem) right.contains(x)
      else true

    def incl(x: Int): IntSet =
      if (x < elem) new NonEmpty(elem, left.incl(x), right)
      else if (x > elem) new NonEmpty(elem, left, right.incl(x))
      else this

    def filter0(p: Int => Boolean, accu: IntSet): IntSet = {
      left filter0(p,
	right filter0(p,
	  if(p(elem))
	    accu incl(elem)
	  else
	    accu
	)
      )
    }
  }



  def main(args : Array[String]) : Unit = {
   println("1 contains 1? (by hand) " + contains((x: Int) => x == 1, 1))
   println("1 contains 1? " + contains(set(1), 1))
   println("2 contains 1? " + contains(set(2), 1))
   println("1 contains 2? " + contains(set(1), 2))

   val s123 = union(set(1), union(set(2), set(3)))
   println("123 contains 1? " + contains(s123, 1))
   println("123 contains 2? " + contains(s123, 2))
   println("123 contains 3? " + contains(s123, 3))
   println("123 contains 4? " + contains(s123, 4))
   println("123 contains 56? " + contains(s123, 56))

   val s13 = diff(s123, set(2))
   println("13 contains 1? " + contains(s13, 1))
   println("13 contains 2? " + contains(s13, 2))
   println("13 contains 3? " + contains(s13, 3))
   println("13 contains 4? " + contains(s13, 4))
   println("13 contains 56? " + contains(s13, 56))

   val s12 = intersect(s123, union(set(1), set(2)))
   println("12 contains 1? " + contains(s12, 1))
   println("12 contains 2? " + contains(s12, 2))
   println("12 contains 3? " + contains(s12, 3))
   println("12 contains 4? " + contains(s12, 4))
   println("12 contains 56? " + contains(s12, 56))

   val s1234 = union(union(union(set(1), set(2)), set(3)), set(4))
   val s24 = filter(s1234, a => a % 2 == 0)
   println("24 contains 1? " + contains(s24, 1))
   println("24 contains 2? " + contains(s24, 2))
   println("24 contains 3? " + contains(s24, 3))
   println("24 contains 4? " + contains(s24, 4))
   println("24 contains 56? " + contains(s24, 56))

   println("24 contains only even numbers? " + forall(s24, a => a % 2 == 0))
   println("1234 contains only even numbers? " + forall(s1234, a => a % 2 == 0))
   println("1234 contains only numbers < 5? " + forall(s1234, a => a < 5))
   println("1234 contains only numbers > 0? " + forall(s1234, a => a > 0))

   println("exists an odd number in 24? "  + exists(s24, a => a % 2 == 1))
   println("exists an even number in 24? " + exists(s24, a => a % 2 == 0))
   println("exists 3 in 1234? " + exists(s1234, a => a == 3))
   println("exists 3 in 12? " + exists(s12, a => a == 3))

   println(toString(s1234))
   println("x2 = " + toString(map(s1234, a => 2 * a)))
   println("x3 = " + toString(map(s1234, a => 3 * a)))
   println("x4 = " + toString(map(s1234, a => 4 * a)))

   val s2345 = new Empty incl(2) incl(3) incl(4) incl(5)
   println(s2345 toString())

   val s4567 = new Empty incl(4) incl(5) incl(6) incl(7)
   println(s4567 toString())

   val s45 = s2345 intersect(s4567)
   println("2345 ^ 4567 = " + s45 toString())

   println("2345, a < 4 = " + (s2345 filter(a => a < 4) toString()))
   println("2345, a > 2 = " + (s2345 filter(a => a > 2) toString()))
   println("2345, a odd  = " + (s2345 filter(a => a % 2 == 1) toString()))
   println("2345, a even = " + (s2345 filter(a => a % 2 == 0) toString()))
  }
}
