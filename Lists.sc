object Lists {
  trait List[+T] {
    def isEmpty: Boolean
    def head: T
    def tail: List[T]
    def nth(n:Int): T
    
    def prepend[U >: T](elem:U): List[U] = new Cons(elem, this)
    
  }
  
  object Nil extends List[Nothing] {
    def isEmpty = true
    def head = throw new NoSuchElementException("Nil.head")
    def tail = throw new NoSuchElementException("Nil.tail")
    def nth(n: Int) = throw new IndexOutOfBoundsException("Nil.nth")
  }
    
  class Cons[T](val head: T, val tail: List[T]) extends List[T] {
    def isEmpty = false
    def nth(n: Int) = if (n== 0) head else tail.nth(n-1)
  }
  

  def nth[T](n: Int, list: List[T]): T =
    if (list.isEmpty) throw new IndexOutOfBoundsException("Nil.nth")
    else if (0 == n) list.head
    else nth(n-1, list.tail)                      //> nth: [T](n: Int, list: Lists.List[T])T
  
  def a = new Cons(5, Nil)                        //> a: => Lists.Cons[Int]
  def b = new Cons(4,a)                           //> b: => Lists.Cons[Int]
  a.nth(0)                                        //> res0: Int = 5
  b.nth(0)                                        //> res1: Int = 4
  //a.nth(1)
  b.nth(1)                                        //> res2: Int = 5
  
  nth(0,a)                                        //> res3: Int = 5
  nth(0,b)                                        //> res4: Int = 4
//  nth(1,a)
  nth(1,b)                                        //> res5: Int = 5
//  nth(-1,b)

  abstract class IntSet {
    def incl(x: Int): IntSet
    def contains(x: Int): Boolean
    def union(other: IntSet): IntSet
    def toString:String
  }
  
  class Empty extends IntSet {
    override def incl(x: Int): IntSet = new NonEmpty(x, new Empty, new Empty)
    override def contains(x: Int): Boolean = false
    override def union(other: IntSet): IntSet = other
    override def toString = "."
  }
  
  class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
    override def contains(x: Int): Boolean = {
      if (x == elem) true
      else if (x < elem) left.contains(x)
      else right.contains(x)
    }
    
    override def toString:String = "{"+left+","+elem+","+right+"}"
    
    override def incl(x: Int): IntSet = {
      if (x == elem) this
      else if (x < elem) new NonEmpty(elem, left.incl(x), right)
      else new NonEmpty(elem, left, right.incl(x))
    }
    
    override def union(other: IntSet): IntSet = {
      other.union(left).union(right).incl(elem)
    }
  }

  def f(xs: List[NonEmpty], x: Empty) = xs prepend x
                                                  //> f: (xs: Lists.List[Lists.NonEmpty], x: Lists.Empty)Lists.List[Lists.IntSet]
                                                  //| 
}