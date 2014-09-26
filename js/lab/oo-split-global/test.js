with (base) {
  console.log('std', interpretProgram(
    app(fun('x', deref(v('x'))),
        ref(c(42))),
    {}, {}
  ));
}

with (facets) {
  console.log('facets', interpretProgram(
    app(fun('x', deref(v('x'))),
        ref(c(42))),
    {}, {}, [1]
  ));
}
