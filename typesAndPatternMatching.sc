object typesAndPatternMatching {
  
  abstract class Bool {
    def ifThenElse[T](t: => T, e: => T): T
    
    def && (x: => Bool): Bool = ifThenElse(x, False)
    def || (x: => Bool): Bool = ifThenElse(True, x)
    def unary_! = ifThenElse(False, True)
    
    def < (x: => Bool): Bool = ifThenElse(False, x)
  }
  
  object True extends Bool {
    def ifThenElse[T](t: => T, e: => T) = t
  }

  object False extends Bool {
    def ifThenElse[T](t: => T, e: => T) = e
  }
  
  abstract class Nat {
    def isZero: Bool
    def predeccessor: Nat
    def successor = new Succ(this)
    def + (that: Nat): Nat
    def - (that: Nat): Nat
    def > (that: Nat): Bool =
      this.isZero.ifThenElse(False, that.isZero.ifThenElse(True, this.predeccessor.> (that.predeccessor)))
  }
  
  object Zero extends Nat {
    def isZero = True
    def predeccessor = throw new RuntimeException("zero.pred")
    def + (that: Nat) = that
    def - (that: Nat) = if (that == Zero) that else throw new RuntimeException("subtract from Zero")
  }
  
  class Succ(n: Nat) extends Nat {
    def isZero = False
    def predeccessor = n
    def + (that: Nat) = this.successor + that.predeccessor
    def - (that: Nat) = that.isZero.ifThenElse(this, this.predeccessor - that.predeccessor)
  }
  
  
}