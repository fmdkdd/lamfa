Set.prototype.union = function(elem) {
  let n = new Set(this);
  n.add(elem);
  return n;
}

function mk_facet(pc, v1, v2) {
  if (pc.size === 0)
    return v1;

  let [k, ...rest] = pc;
  rest = new Set(rest);

  if (k > 0)
    return facet(k, mk_facet(rest, v1, v2), v2);
  else
    return facet(k, v2, mk_facet(rest, v1, v2));
}

function facet(k, vh, vl) {
  return {
    eval_apply(σ, pc, v2) {
      if (pc.has(k)) {
        return vh.eval_apply(σ, pc, v2);
      }

      else if (pc.has(-k)) {
        return vl.eval_apply(σ, pc, v2);
      }

      else {
        let [σ1, vh1] = vh.eval_apply(σ, pc.union(k), v2);
        let [σ2, vl1] = vl.eval_apply(σ1, pc.union(-k), v2);
        return [ σ2, mk_facet(k, vh1, vl1) ];
      }
    },

    eval_deref(σ, pc) {
      if (pc.has(k))
        return vh.eval_deref(σ, pc);
      else if (pc.has(-k))
        return vl.eval_deref(σ, pc);
      else
        return mk_facet(k, vh.eval_deref(σ, pc), vl.eval_deref(σ, pc));
    },

    eval_assign(σ, pc, v) {
      let σ1 = vh.eval_assign(σ, pc.union(k), v);
      return vl.eval_assign(σ1, pc.union(-k), v);
    }
  };
}

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
    eval_apply(σ, pc, v) {
      let θ1 = Object.create(θ);
      θ1[x] = v;
      return e.eval(σ, θ1, pc);
    }
  };
}

function c(e) {
  return {
    eval(σ, θ, pc) {
      return [ σ, e ];
    }
  };
}

function v(e) {
  return {
    eval(σ, θ, pc) {
      return [ σ, θ[e] ];
    }
  };
}

function fun(x, e) {
  return {
    eval(σ, θ, pc) {
      return [ σ, closure(x, e, θ) ];
    }
  };
}

function app(e1, e2) {
  return {
    eval(σ, θ, pc) {
      let [σ1, v1] = e1.eval(σ, θ, pc);
      let [σ2, v2] = e2.eval(σ1, θ, pc);
      return v1.eval_apply(σ2, pc, v2);
    }
  };
}

function ref(e) {
  return {
    eval(σ, θ, pc) {
      let [σ1, v] = e.eval(σ, θ, pc);
      let a = Object.keys(σ1).length;
      let σ2 = Object.create(σ1);
      σ2[a] = mk_facet(pc, v, bottom);
      return [ σ2, address(a) ];
    }
  };
}

function deref(e) {
  return {
    eval(σ, θ, pc) {
      let [σ1, v] = e.eval(σ, θ, pc);
      return [ σ1, v.eval_deref(σ1, pc, v) ];
    }
  };
}

function assign(e1, e2) {
  return {
    eval(σ, θ, pc) {
      let [σ1, v1] = e1.eval(σ, θ, pc);
      let [σ2, v2] = e2.eval(σ1, θ, pc);
      return [ v1.eval_assign(σ2, pc, v2), v2 ];
    }
  };
}

function interpretProgram(AST, env = {}, store = {}, pc = []) {
  let pc = new Set(pc);
  return AST.eval(env, store, pc);
}

// Test
interpretProgram(
  app(fun('x', deref(v('x'))),
      ref(c(42))),
  {}, {}, [1]
);
