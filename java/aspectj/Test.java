import java.util.*;

interface Eval {
  Result eval(Store s, Env e);
}

interface Value {
  Result evalApply(Store s, Value v);
  Value evalDeref(Store s);
}

class Result {
  private Store s;
  private Value v;

  public Result(Store s, Value v) {
    this.s = s;
    this.v = v;
  }

  public Store store() { return s; }
  public Value value() { return v; }
}

class Store {
  private List<Value> vals;

  public Store() {
    vals = new ArrayList();
  }

  public int length() { return vals.size(); }
  public Value get(Address a) { return vals.get(a.n()); }
  public Store add(Value v) {
    Store s = new Store();
    s.vals = new ArrayList(this.vals);
    s.vals.add(v);
    return s;
  }

  public String toString() {
    return vals.toString();
  }
}

class Env {
  private Map<String,Value> vals;

  public Env() {
    vals = new HashMap();
  }

  public Value get(String n) { return vals.get(n); }
  public Env add(String n, Value v) {
    Env e = new Env();
    e.vals = new HashMap(this.vals);
    e.vals.put(n, v);
    return e;
  }
}

class Bottom implements Value {
  public Result evalApply(Store s, Value v) {
    return new Result(s, this);
  }

  public Value evalDeref(Store s) {
    return this;
  }

  public String toString() {
    return "Bottom";
  }
}

class Address implements Value {
  int n;

  public Address(int n) {
    this.n = n;
  }

  public int n() { return n; }

  public Result evalApply(Store s, Value v) {
    throw new UnsupportedOperationException();
  }

  public Value evalDeref(Store s) {
    return s.get(this);
  }

  public String toString() {
    return "Address " + n;
  }
}

class Closure implements Value {
  String x;
  Eval e;
  Env env;

  public Closure(String x, Eval e, Env env) {
    this.x = x;
    this.e = e;
    this.env = env;
  }

  public Result evalApply(Store s, Value v) {
    Env env1 = env.add(x, v);
    return e.eval(s, env1);
  }

  public Value evalDeref(Store s) {
    throw new UnsupportedOperationException();
  }
}

class Const implements Eval, Value {
  int n;

  public Const(int n) {
    this.n = n;
  }

  public Result eval(Store s, Env e) {
    return new Result(s, this);
  }

  public Result evalApply(Store s, Value v) {
    throw new UnsupportedOperationException();
  }

  public Value evalDeref(Store s) {
    throw new UnsupportedOperationException();
  }

  public String toString() {
    return "" + n;
  }
}

class Var implements Eval {
  String x;

  public Var(String x) {
    this.x = x;
  }

  public Result eval(Store s, Env e) {
    return new Result(s, e.get(x));
  }
}

class Fun implements Eval {
  String x;
  Eval e;

  public Fun(String x, Eval e) {
    this.x = x;
    this.e = e;
  }

  public Result eval(Store s, Env env) {
    return new Result(s, new Closure(x, e, env));
  }
}

class App implements Eval {
  Eval e1;
  Eval e2;

  public App(Eval e1, Eval e2) {
    this.e1 = e1;
    this.e2 = e2;
  }

  public Result eval(Store s, Env env) {
    Result r1 = e1.eval(s, env);
    Result r2 = e2.eval(r1.store(), env);
    return r1.value().evalApply(r2.store(), r2.value());
  }
}

class Ref implements Eval {
  Eval e;

  public Ref(Eval e) {
    this.e = e;
  }

  public Result eval(Store s, Env env) {
    Result r1 = e.eval(s, env);
    Address a = new Address(r1.store().length());
    Store s2 = r1.store().add(r1.value());
    return new Result(s2, a);
  }
}

class Deref implements Eval {
  Eval e;

  public Deref(Eval e) {
    this.e = e;
  }

  public Result eval(Store s, Env env) {
    Result r1 = e.eval(s, env);
    return new Result(r1.store(), r1.value().evalDeref(r1.store()));
  }
}

class Test {
  public static void main(String[] args) {
    Eval e = new App(new Fun("x", new Deref(new Var("x"))),
                     new Ref(new Const(42)));
    Result r = e.eval(new Store(), new Env());
    System.out.println(r.store());
    System.out.println(r.value());
  }
}
