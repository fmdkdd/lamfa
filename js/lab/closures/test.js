with (base) {
  console.log(interpretProgram(
    app(fun('x', deref(v('x'))),
        ref(c(42)))
  ));
}

with (facets) {
  console.log(interpretProgram(
    app(fun('x', deref(v('x'))),
        ref(c(42))),
    {}, {}, [1]
  ));
}

with (base) {
  console.log(interpretProgram(
    app(fun('x', deref(v('x'))),
        ref(c(42)))
  ));
}
