Set.prototype.union = function(elem) {
  let n = new Set(this);
  n.add(elem);
  return n;
}

function interpretNode(σ, θ, pc, node) {
  return rules[node.type](σ, θ, pc, node);
}

let ↆ = interpretNode;

let bottom = {type: 'bottom'};

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
function closure(x, e, θ) { return {type: 'closure', x, e, θ}; }
function address(a) { return {type: 'address', a}; }

function eval_apply(σ, pc, v1, v2) {
  return application_rules[v1.type](σ, pc, v1, v2);
}

let application_rules = {
  bottom(σ) {
    return [σ, bottom];
  },

  closure(σ, pc, {x, e, θ}, v) {
    let θ1 = Object.create(θ);
    θ1[x] = v;
    return ↆ(σ, θ1, pc, e);
  },

  facet(σ, pc, {k, vh, vl}, v2) {
    if (pc.has(k)) {
      return eval_apply(σ, pc, vh, v2);
    }

    else if (pc.has(-k)) {
      return eval_apply(σ, pc, vl, v2);
    }

    else {
      let [σ1, vh1] = eval_apply(σ, pc.union(k), vh, v2);
      let [σ2, vl1] = eval_apply(σ1, pc.union(-k), vl, v2);
      return [ σ2, mk_facet(k, vh1, vl1) ];
    }
  },
};

function eval_deref(σ, v, pc) {
  return deref_rules[v.type](σ, v, pc);
}

let deref_rules = {
  bottom() {
    return bottom;
  },

  address(σ, {a}, pc) {
    return σ[a];
  },

  facet(σ, {k, vh, vl}, pc) {
    if (pc.has(k))
      return eval_deref(σ, vh, pc);
    else if (pc.has(-k))
      return eval_deref(σ, vl, pc);
    else
      return mk_facet(k, eval_deref(σ, vh, pc), eval_deref(σ, vl, pc));
  },
};

function eval_assign(σ, pc, v1, v2) {
  return assign_rules[v1.type](σ, pc, v1, v2);
}

let assign_rules = {
  bottom(σ) {
    return σ;
  },

  address(σ, pc, {a}, v) {
    let σ2 = Object.create(σ);
    σ2[a] = mk_facet(pc, v, σ[a]);
    return σ2;
  },

  facet(σ, pc, {k, vh, vl}, v) {
    let σ1 = eval_assign(σ, pc.union(k), vh, v);
    return eval_assign(σ1, pc.union(-k), vl, v);
  },
};

let rules = {
  c(σ, θ, pc, {e}) {
    return [ σ, e ];
  },

  v(σ, θ, pc, {e}) {
    return [ σ, θ[e] ];
  },

  fun(σ, θ, pc, {x, e}) {
    return [ σ, closure(x, e, θ) ];
  },

  app(σ, θ, pc, {e1, e2}) {
    let [σ1, v1] = ↆ(σ, θ, pc, e1);
    let [σ2, v2] = ↆ(σ1, θ, pc, e2);
    return eval_apply(σ2, pc, v1, v2);
  },

  ref(σ, θ, pc, {e}) {
    let [σ1, v] = ↆ(σ, θ, pc, e);
    let a = Object.keys(σ1).length;
    let σ2 = Object.create(σ1);
    σ2[a] = mk_facet(pc, v, bottom);
    return [ σ2, address(a) ];
  },

  deref(σ, θ, pc, {e}) {
    let [σ1, v] = ↆ(σ, θ, pc, e);
    return [ σ1, eval_deref(σ1, v, pc) ];
  },

  assign(σ, θ, pc, {e1, e2}) {
    let [σ1, v1] = ↆ(σ, θ, pc, e1);
    let [σ2, v2] = ↆ(σ1, θ, pc, e2);
    return [ eval_assign(σ2, pc, v1, v2), v2 ];
  },
};

function interpretProgram(AST, env = {}, store = {}, pc = []) {
  let pc = new Set(pc);
  return interpretNode(env, store, pc, AST);
}

// Test
function app(e1, e2) { return {type: 'app', e1, e2}; }
function fun(x, e) { return {type: 'fun', x, e}; }
function ref(e) { return {type: 'ref', e}; }
function deref(e) { return {type: 'deref', e}; }
function c(e) { return {type: 'c', e}; }
function v(e) { return {type: 'v', e}; }

interpretProgram(
  app(fun('x', deref(v('x'))),
      ref(c(42))),
  {}, {}, [1]
);
