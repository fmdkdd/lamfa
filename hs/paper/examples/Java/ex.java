interface Term {
    Value interp();
}

class Constant implements Term {
    int i = 0;
    Constant(int i) { this.i = i; }
    public Value interp() { return new Number(this.i); }
}

interface Value<T> {
    T get();
}

class Number implements Value<Integer> {
    int i;
    Number(int i) { this.i = i; }
    public Integer get() { return this.i; }
}

class Plus implements Term {
    Term t1, t2;
    Plus(Term t1, Term t2) { this.t1 = t1; this.t2 = t2; }
    public Value interp() { return plus(t1.interp(), t2.interp()); }

    public Value plus(Number a, Number b) {
        return a + b;
    }
}

class Constant42a extends Constant {
    @Override
    public Value interp() { Value r = super.interp(); return new Number(r.get()); }
}

class Constant42b extends Constant {
    @Override
    public Value interp() { this.i *= 42; return super.interp(); }
}
