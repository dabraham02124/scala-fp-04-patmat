object patmat {
  trait Expr {
    def eval: Int = this match {
      case Num(n) => n
      case Sum(e1, e2) => e1.eval + e2.eval
    }
    def show: String = this match {
      case Num(n) => n.toString
      case Sum(e1, e2) => e1.show + " + " + e2.show
      case Var(n) => n
      case Prod(Sum(e1, e2), e3) => "("+Sum(e1, e2).show+") * "+ e3.show
      case Prod(e1, e2) => e1.show + " * " + e2.show
    }
  }
  
  case class Num(n:Int) extends Expr
  case class Sum(e1: Expr, e2: Expr) extends Expr
  case class Var(name: String) extends Expr
  case class Prod(e1: Expr, e2: Expr) extends Expr
  
  val opt = Sum(Num(1), Num(2))                   //> opt  : patmat.Sum = Sum(Num(1),Num(2))
  opt.eval                                        //> res0: Int = 3
  opt.show                                        //> res1: String = 1 + 2
  Sum(Prod(Num(2), Var("x")), Var("y")).show      //> res2: String = 2 * x + y
  Prod(Sum(Num(2), Var("x")), Var("y")).show      //> res3: String = (2 + x) * y
}