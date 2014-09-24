let bottom = {
  eval_apply(C) {
    return [ C, bottom ];
  },

  eval_deref() {
    return bottom;
  },

  eval_assign(C) {
    return C;
  }
};

function address(a) {
  return {
    eval_deref(C) {
      return C.σ[a];
    },

    eval_assign(C, v) {
      let C1 = with_ctxt(C, {σ: Object.create(C.σ)});
      C1.σ[a] = v;
      return C1;
    }
  };
}

function closure(x, e, θ) {
  return {
    eval_apply(C, v) {
      let C1 = with_ctxt(C, {θ: Object.create(C.θ)});
      C1.θ[x] = v;
      return e.eval(C1);
    }
  };
}

function c(e) {
  return {
    eval(C) {
      return [ C, e ];
    }
  };
}

function v(e) {
  return {
    eval(C) {
      return [ C, C.θ[e] ];
    }
  };
}

function fun(x, e) {
  return {
    eval(C) {
      return [ C, closure(x, e, C.θ) ];
    }
  };
}

function app(e1, e2) {
  return {
    eval(C) {
      let [C1, v1] = e1.eval(C);
      let [C2, v2] = e2.eval(C1);
      return v1.eval_apply(C2, v2);
    }
  };
}

function ref(e) {
  return {
    eval(C) {
      let [C1, v] = e.eval(C);
      let a = Object.keys(C1.σ).length;
      let C2 = with_ctxt(C1, {σ: Object.create(C1.σ)});
      C2.σ[a] = mk_facet(C1.pc, v, bottom);
      return [ C2, address(a) ];
    }
  };
}

function deref(e) {
  return {
    eval(C) {
      let [C1, v] = e.eval(C);
      return [ C1, v.eval_deref(C1) ];
    }
  };
}

function assign(e1, e2) {
  return {
    eval(C) {
      let [C1, v1] = e1.eval(C);
      let [C2, v2] = e2.eval(C1);
      return [ v1.eval_assign(C2, v2), v2 ];
    }
  };
}

function with_ctxt(C, obj) {
  obj.__proto__ = C;
  return obj;
}

function interpretProgram(AST, env = {}, store = {}) {
  return AST.eval({σ: env, θ: store});
}

// Test
// console.log(interpretProgram(
//   app(fun('x', deref(v('x'))),
//       ref(c(42))),
//   {}, {}
// ));

console.log(interpretFacetProgram(
  app(fun('x', deref(v('x'))),
      ref(c(42))),
  {}, {}, [1]
));
