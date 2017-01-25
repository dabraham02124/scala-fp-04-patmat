object Lists2 {
  val fruit = List("apples", "oranges", "pears")  //> fruit  : List[String] = List(apples, oranges, pears)
  val nums = List(1,2,3,4)                        //> nums  : List[Int] = List(1, 2, 3, 4)
  val diag3 = List(List(1,0,0),List(0,1,0),List(0,0,1))
                                                  //> diag3  : List[List[Int]] = List(List(1, 0, 0), List(0, 1, 0), List(0, 0, 1))
                                                  //| 
  val empty = List()                              //> empty  : List[Nothing] = List()
  
  def insert(i: Int, l: List[Int]): List[Int] = l match {
    case List() => List(i)
    case y :: ys =>
      if (i < y) i :: y :: ys
      else y :: insert(i, ys)
  }                                               //> insert: (i: Int, l: List[Int])List[Int]
  
  def iSort(l: List[Int]): List[Int] = l match {
    case List() => List()
    case x :: xs => insert(x, iSort(xs))
  }                                               //> iSort: (l: List[Int])List[Int]
  
  
  val ooo = List(4,2,7,1,9)                       //> ooo  : List[Int] = List(4, 2, 7, 1, 9)
  val ooo2 = iSort(ooo)                           //> ooo2  : List[Int] = List(1, 2, 4, 7, 9)
}