import java.util.ArrayList;
import java.util.Hashtable;

class Env {
  Hashtable<String, Object> env;
  public Env() { env = new Hashtable<>(); }
  Env put(String name, Object o) {
    Env n = new Env();
    n.env.putAll(env);
    n.env.put(name, o);
    return n;
  }
  Object get(String name) { return env.get(name); }
}

class Store {
  ArrayList<Object> store;
  public Store() { store = new ArrayList<>(); }
  Address freeAddress() { return new Address(store.size()); }
  Store put(Address a, Object o) {
    Store s = new Store();
    s.store.addAll(store);
    s.store.add(a.a, o);
    return s;
  }
  Object get(Address a) { return store.get(a.a); }
}

class Context {
  Env env; Store store;
  public Context(Env env, Store store) { this.env = env; this.store = store; }
}

class EvalResult {
  Context c; Object v;
  public EvalResult(Context c, Object v) { this.c = c; this.v = v; }
}

class Address {
  int a;
  public Address(int a) { this.a = a; }
  Object evalDeref(Context c) {
    return c.store.get(this);
  }
}

class Closure extends Object {
  String x; Node body; Env env;
  public Closure(String x, Node body, Env env) { this.x = x; this.body = body; this.env = env; }
  EvalResult evalApply(Context c, Object v) {
    return body.eval(new Context(env.put(x, v), c.store));
  }
}

interface Node {
  EvalResult eval(Context c);
}

class C implements Node {
  int v;
  public C(int v) { this.v = v; }
  public EvalResult eval(Context c) { return new EvalResult(c, this.v); }
}

class V implements Node {
  String name;
  public V(String name) { this.name = name; }
  public EvalResult eval(Context c) { return new EvalResult(c, c.env.get(name)); }
}

class Fun implements Node {
  String x; Node body;
  public Fun(String x, Node body) { this.x = x; this.body = body; }
  public EvalResult eval(Context c) { return new EvalResult(c, new Closure(x, body, c.env)); }
}

class App implements Node {
  Node e1, e2;
  public App(Node e1, Node e2) { this.e1 = e1; this.e2 = e2; }
  public EvalResult eval(Context c) {
    EvalResult r1 = e1.eval(c);
    EvalResult r2 = e2.eval(r1.c);
    return ((Closure)r1.v).evalApply(r2.c, r2.v);
  }
}

class Ref implements Node {
  Node e;
  public Ref(Node e) { this.e = e; }
  public EvalResult eval(Context c) {
    EvalResult r = e.eval(c);
    Address a = r.c.store.freeAddress();
    Store s = r.c.store.put(a, r.v);
    return new EvalResult(new Context(c.env, s), a);
  }
}

class Deref implements Node {
  Node e;
  public Deref(Node e) { this.e = e; }
  public EvalResult eval(Context c) {
    EvalResult r = e.eval(c);
    return new EvalResult(c, ((Address)r.v).evalDeref(c));
  }
}

class Test {
  public static void main(String args[]) {
    Node n = new App(new Fun("x", new Deref(new V("x"))),
                     new Ref(new C(42)));
    EvalResult r = n.eval(new Context(new Env(), new Store()));
    System.out.println(r.v);
  }
}

// Facets

class TestFacets {
  public static void main(String args[]) {
    Node n = new App(new Fun("x", new Deref(new V("x"))),
                     new Ref(new C(42)));
    EvalResult r = n.eval(new Context(new Env(), new Store()));
    System.out.println(r.v);
  }
}
