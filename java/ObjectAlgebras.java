abstract class Term { abstract int eval(); }

class Constant extends Term {
  int n;
  Constant(int n) { this.n = n; }
  public int eval() { return n; }
}

class Plus extends Term {
  Term a, b;
  Plus(Term a, Term b) { this.a = a; this.b = b; }
  public int eval() { return a.eval() + b.eval(); }
}

interface Visitor<T> {
  T constant(int n);
  T plus(T e1, T e2);
}

class ASTVisitor implements Visitor<Term> {
  public Term constant(int n) {
    return new Constant(n);
  }

  public Term plus(Term a, Term b) {
    return new Plus(a, b);
  }
}

interface PP { String pp(); }

class PPVisitor implements Visitor<PP> {
  public PP constant(int n) {
    return () -> new Integer(n).toString();
  }

  public PP plus(PP a, PP b) {
    return () -> a.pp() + " + " + b.pp();
  }
}

// Can't use lambdas to create expressions
// interface Expr {
//   <T> T build(Visitor<T> v);
// }

class Test {
  public static void main(String args[]) {
    ASTVisitor astv = new ASTVisitor();
    PPVisitor ppv = new PPVisitor();

    Term e1 = new Plus(new Constant(1), new Constant(2));
    System.out.println(e1.eval());

    System.out.println(e2(astv).eval());
    System.out.println(e2(ppv).pp());

    System.out.println(e3(new ASTVisitorForMult()).eval());
    System.out.println(e3(new PPVisitorForMult()).pp());
  }

  static <T> T e2(Visitor<T> v) {
    return v.plus(v.constant(1), v.constant(2));
  }

  static <T> T e3(VisitorForMult<T> v) {
    return v.plus(v.mult(v.constant(1), v.constant(2)), v.constant(1));
  }
}

/* Add Mult */
class Mult extends Term {
  Term a, b;
  Mult(Term a, Term b) { this.a = a; this.b = b; }
  public int eval() { return a.eval() * b.eval(); }

}

interface VisitorForMult<T> extends Visitor<T> {
  T mult(T e1, T e2);
}

class ASTVisitorForMult extends ASTVisitor implements VisitorForMult<Term> {
  public Term mult(Term e1, Term e2) {
    return new Mult(e1, e2);
  }
}

class PPVisitorForMult extends PPVisitor implements VisitorForMult<PP> {
  public PP mult(PP e1, PP e2) {
    return () -> e1.pp() + " * " + e2.pp();
  }
}
