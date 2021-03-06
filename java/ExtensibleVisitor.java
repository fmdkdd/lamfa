abstract class Term { abstract <T> T accept(Visitor<T> v); }

class Constant extends Term {
  int n;
  Constant(int n) { this.n = n; }
  <T> T accept(Visitor<T> v) { return v.forConstant(this); }
}

class Plus extends Term {
  Term a, b;
  Plus(Term a, Term b) { this.a = a; this.b = b; }
  <T> T accept(Visitor<T> v) { return v.forPlus(this); }
}

interface Visitor<T> {
  T forConstant(Constant c);
  T forPlus(Plus p);
}

class Eval implements Visitor<Integer> {
  public Integer forConstant(Constant c) { return c.n; }
  public Integer forPlus(Plus p) { return p.a.accept(this) + p.b.accept(this); }
}

class Test {
  public static void main(String args[]) {
    Term e1 = new Plus(new Constant(1), new Constant(2));
    System.out.println(e1.accept(new Eval()));

    Term e2 = new Plus(new Mult(new Constant(1), new Constant(2)), new Constant(1));
    System.out.println(e2.accept(new EvalForMult()));
  }
}

/* Add Mult */
class Mult extends Term {
  Term a, b;
  Mult(Term a, Term b) { this.a = a; this.b = b; }
  <T> T accept(Visitor<T> v) { return ((VisitorForMult<T>)v).forMult(this); }
}

interface VisitorForMult<T> extends Visitor<T> {
  T forMult(Mult m);
}

class EvalForMult extends Eval implements VisitorForMult<Integer> {
  public Integer forMult(Mult m) {
    return m.a.accept(this) * m.b.accept(this);
  }
}
