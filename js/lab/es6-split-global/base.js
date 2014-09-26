(function(){

Set.prototype.union = function(elem) {
  let n = new Set(this);
  n.add(elem);
  return n;
}

function interpretNode(σ, θ, node) {
  return rules[node.type](σ, θ, node);
}

let ↆ = interpretNode;

let bottom = {type: 'bottom'};

function closure(x, e, θ) { return {type: 'closure', x, e, θ}; }
function address(a) { return {type: 'address', a}; }

function eval_apply(σ, v1, v2) {
  return application_rules[v1.type](σ, v1, v2);
}

let application_rules = {
  bottom(σ) {
    return [σ, bottom];
  },

  closure(σ, {x, e, θ}, v) {
    let θ1 = Object.create(θ);
    θ1[x] = v;
    return ↆ(σ, θ1, e);
  },
};

function eval_deref(σ, v) {
  return deref_rules[v.type](σ, v);
}

let deref_rules = {
  bottom() {
    return bottom;
  },

  address(σ, {a}) {
    return σ[a];
  },
};

function eval_assign(σ, v1, v2) {
  return assign_rules[v1.type](σ, v1, v2);
}

let assign_rules = {
  bottom(σ) {
    return σ;
  },

  address(σ, {a}, v) {
    let σ2 = Object.create(σ);
    σ2[a] = v;
    return σ2;
  },
};

let rules = {
  c(σ, θ, {e}) {
    return [ σ, e ];
  },

  v(σ, θ, {e}) {
    return [ σ, θ[e] ];
  },

  fun(σ, θ, {x, e}) {
    return [ σ, closure(x, e, θ) ];
  },

  app(σ, θ, {e1, e2}) {
    let [σ1, v1] = ↆ(σ, θ, e1);
    let [σ2, v2] = ↆ(σ1, θ, e2);
    return eval_apply(σ2, v1, v2);
  },

  ref(σ, θ, {e}) {
    let [σ1, v] = ↆ(σ, θ, e);
    let a = Object.keys(σ1).length;
    let σ2 = Object.create(σ1);
    σ2[a] = v;
    return [ σ2, address(a) ];
  },

  deref(σ, θ, {e}) {
    let [σ1, v] = ↆ(σ, θ, e);
    return [ σ1, eval_deref(σ1, v) ];
  },

  assign(σ, θ, {e1, e2}) {
    let [σ1, v1] = ↆ(σ, θ, e1);
    let [σ2, v2] = ↆ(σ1, θ, e2);
    return [ eval_assign(σ2, v1, v2), v2 ];
  },
};

function interpretProgram(AST, env = {}, store = {}) {
  return interpretNode(env, store, AST);
}

// AST
function app(e1, e2) { return {type: 'app', e1, e2}; }
function fun(x, e) { return {type: 'fun', x, e}; }
function ref(e) { return {type: 'ref', e}; }
function deref(e) { return {type: 'deref', e}; }
function assign(e1, e2) { return {type: 'assign', e1, e2}; }
function c(e) { return {type: 'c', e}; }
function v(e) { return {type: 'v', e}; }

// Exports
this.base = {
  interpretProgram,
  c, v, fun, app, ref, deref, assign,
  _innards: { application_rules, deref_rules, assign_rules, rules,
              interpretNode,
              bottom, address }
};

}());
