import java.util.*;

interface Term { int eval(); }

class Num implements Term {
  int n;

  Num(int n) { this.n = n; }

  public int eval() { return n; }
}

class Plus implements Term {
  Term a, b;

  Plus(Term a, Term b) {
    this.a = a; this.b = b;
  }

  public int eval() {
    return a.eval() + b.eval();
  }
}

class Test {
  public static void main(String[] args) {
    Term e1 = new Plus(new Num(1), new Num(2));
    System.out.println(e1.eval());

    Term e2 = new Plus(
                       new Trace(new Plus(new Num(1), new Num(2))),
                       new Num(3));
    System.out.println(e2.eval());
  }
}

class Trace implements Term {
  Term e;

  Trace(Term e) {
    this.e = e;
  }

  public int eval() {
    return e.eval();
  }
}

aspect DynamicTrace {
  pointcut tracedEval(Term t):
  call(int eval()) && target(t)
    && cflow(call(int eval()) && target(Trace));

  before(Term t): tracedEval(t) {
    System.out.println("eval " + t);
  }
}
