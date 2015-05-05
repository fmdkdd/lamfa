import java.util.*;

aspect Facets {
  static PC pc = null; //new PC().add(1);

  class PC {
    private List<Integer> vals;

    public PC() {
      vals = new ArrayList();
    }

    public boolean has(int v) {
      return vals.contains(v);
    }

    public PC add(int k) {
      PC p = new PC();
      p.vals = new ArrayList(this.vals);
      p.vals.add(k);
      return p;
    }

    public boolean isEmpty() { return vals.isEmpty(); }
    public int head() { return vals.get(0); }
    public PC tail() {
      PC p = new PC();
      p.vals = new ArrayList(vals.subList(1, vals.size()));
      return p;
    }
  }

  class Facet implements Value {
    int k;
    Value vh;
    Value vl;

    public Facet(int k, Value vh, Value vl) {
      this.k = k;
      this.vh = vh;
      this.vl = vl;
    }

    public Result evalApply(Store s, Value v) {
      if (pc.has(k))
        return vh.evalApply(s, v);
      else if (pc.has(-k))
        return vl.evalApply(s, v);
      else {
        PC old_pc = pc;
        pc = old_pc.add(k);
        Result rh = vh.evalApply(s, v);
        pc = old_pc.add(-k);
        Result rl = vl.evalApply(rh.store(), v);
        pc = old_pc;
        return new Result( rl.store(),
                           mkFacet(new PC().add(k),
                                   rh.value(),
                                   rl.value()));
      }
    }

    public Value evalDeref(Store s) {
      if (pc.has(k))
        return vh.evalDeref(s);
      else if (pc.has(-k))
        return vl.evalDeref(s);
      else
        return mkFacet(new PC().add(k), vh.evalDeref(s), vl.evalDeref(s));
    }

    public String toString() {
      return "<" + k + " ? " + vh + " : " + vl + ">";
    }
  }

  Value mkFacet(PC pc, Value v1, Value v2) {
    if (pc.isEmpty())
      return v1;

    int k = pc.head();

    if (k > 0)
      return new Facet(k, mkFacet(pc.tail(), v1, v2), v2);
    else
      return new Facet(k, v2, mkFacet(pc.tail(), v1, v2));
  }

  pointcut ref(Ref ref, Store s, Env env):
  call(Result eval(Store, Env))
    && target(ref)
    && args(s, env);

  Result around(Ref ref, Store s, Env env): ref(ref, s, env) {
    System.out.println("aspect");
    Result r1 = ref.e.eval(s, env);
    Address a = new Address(r1.store().length());
    Value v = mkFacet(pc, r1.value(), new Bottom());
    Store s2 = r1.store().add(v);
    return new Result(s2, a);
  }

  pointcut entry(): execution(void Test.main(String[]));

  before(): entry() {
    pc = Facets.aspectOf().new PC().add(1);
  }

}
