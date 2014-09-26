(function() {

let bottom = base._innards.bottom;
let address = base._innards.address;
let ↆ = base._innards.interpretNode;

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

function facet(k, vh, vl) { return {type: 'facet', k, vh, vl}; }

base._innards.application_rules.facet = (σ, {k, vh, vl}, v2) => {
  if (pc.has(k)) {
    return eval_apply(σ, vh, v2);
  }

  else if (pc.has(-k)) {
    return eval_apply(σ, vl, v2);
  }

  else {
    let [σ1, vh1] = with_pc(pc.union(k), () => eval_apply(σ, vh, v2));
    let [σ2, vl1] = with_pc(pc.union(-k), () => eval_apply(σ1, vl, v2));
    return [ σ2, mk_facet(k, vh1, vl1) ];
  }
};

base._innards.deref_rules.facet = (σ, {k, vh, vl}) => {
  if (pc.has(k))
    return eval_deref(σ, vh);
  else if (pc.has(-k))
    return eval_deref(σ, vl);
  else
    return mk_facet(k, eval_deref(σ, vh), eval_deref(σ, vl));
};

base._innards.assign_rules.facet = (σ, {k, vh, vl}, v) => {
  let σ1 = with_pc(pc.union(k), () => eval_assign(σ, vh, v));
  return with_pc(pc.union(-k), () => eval_assign(σ1, vl, v));
};

function with_pc(new_pc, thunk) {
  let old_pc = pc;
  pc = new_pc;
  let ret = thunk();
  pc = old_pc;
  return ret;
}

let pc = null;

function interpretProgram(AST, env = {}, store = {}, default_pc = []) {
  let old_ref = base._innards.rules.ref;
  base._innards.rules.ref = (σ, θ, {e}) => {
    let [σ1, v] = ↆ(σ, θ, e);
    let a = Object.keys(σ1).length;
    let σ2 = Object.create(σ1);
    σ2[a] = mk_facet(pc, v, bottom);
    return [ σ2, address(a) ];
  };
  let r = with_pc(new Set(default_pc), () =>
                  base.interpretProgram(AST, env, store));
  base._innards.rules.ref = old_ref;
  return r;
}

// Exports
this.facets = {
  __proto__: this.base,
  interpretProgram,
};

}());
