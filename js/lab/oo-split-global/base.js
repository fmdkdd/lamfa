let bottom = {
  eval_apply(σ) {
    return [ σ, bottom ];
  },

  eval_deref() {
    return bottom;
  },

  eval_assign(σ) {
    return σ;
  }
};

function address(a) {
  return {
    eval_deref(σ) {
      return σ[a];
    },

    eval_assign(σ, v) {
      let σ2 = Object.create(σ);
      σ2[a] = v;
      return σ2;
    }
  };
}

function closure(x, e, θ) {
  return {
    eval_apply(σ, v) {
      let θ1 = Object.create(θ);
      θ1[x] = v;
      return e.eval(σ, θ1);
    }
  };
}

function c(e) {
  return {
    eval(σ, θ) {
      return [ σ, e ];
    }
  };
}

function v(e) {
  return {
    eval(σ, θ) {
      return [ σ, θ[e] ];
    }
  };
}

function fun(x, e) {
  return {
    eval(σ, θ) {
      return [ σ, closure(x, e, θ) ];
    }
  };
}

function app(e1, e2) {
  return {
    eval(σ, θ) {
      let [σ1, v1] = e1.eval(σ, θ);
      let [σ2, v2] = e2.eval(σ1, θ);
      return v1.eval_apply(σ2, v2);
    }
  };
}

function ref(e) {
  return {
    eval(σ, θ) {
      let [σ1, v] = e.eval(σ, θ);
      let a = Object.keys(σ1).length;
      let σ2 = Object.create(σ1);
      σ2[a] = mk_facet(pc, v, bottom);
      return [ σ2, address(a) ];
    }
  };
}

function deref(e) {
  return {
    eval(σ, θ) {
      let [σ1, v] = e.eval(σ, θ);
      return [ σ1, v.eval_deref(σ1, v) ];
    }
  };
}

function assign(e1, e2) {
  return {
    eval(σ, θ) {
      let [σ1, v1] = e1.eval(σ, θ);
      let [σ2, v2] = e2.eval(σ1, θ);
      return [ v1.eval_assign(σ2, v2), v2 ];
    }
  };
}

function interpretProgram(AST, env = {}, store = {}) {
  return AST.eval(env, store);
}

// Test
console.log(interpretProgram(
  app(fun('x', deref(v('x'))),
      ref(c(42))),
  {}, {}
));

console.log(interpretFacetProgram(
  app(fun('x', deref(v('x'))),
      ref(c(42))),
  {}, {}, [1]
));
