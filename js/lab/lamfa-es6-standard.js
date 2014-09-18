function interpretNode(σ, θ, node) {
  return rules[node.type](σ, θ, node);
}

let ↆ = interpretNode;

let bottom = {type: 'bottom'};

let application_rules = {
  bottom: function(σ) {
    return [σ, bottom];
  },

  closure: function(σ, {x, e, θ}, v) {
    let θ1 = Object.create(θ);
    θ1[x] = v;
    return ↆ(σ, θ1, e);
  },
};

let deref_rules = {
  bottom: function() {
    return bottom;
  },

  address: function(σ, {a}) {
    return σ[a];
  }
};

let assign_rules = {
  bottom: function(σ) {
    return σ;
  },

  address: function(σ, {a}, v) {
    let σ2 = Object.create(σ);
    σ2[a] = v;
    return σ2;
  },
};

let rules = {
  c: function(σ, θ, {c}) {
    return [ σ, c ];
  },

  v: function(σ, θ, {x}) {
    return [ σ, θ[x] ];
  },

  fun: function(σ, θ, {x, e}) {
    return [ σ, {type: 'closure', x, e, θ} ];
  },

  app: function(σ, θ, {e1, e2}) {
    let [σ1, v1] = ↆ(σ, θ, e1);
    let [σ2, v2] = ↆ(σ1, θ, e2);
    return application_rules[v1.type](σ2, v1, v2);
  },

  ref: function(σ, θ, {e}) {
    let [σ1, v] = ↆ(σ, θ, e);
    let a = Object.keys(σ).length;
    let σ2 = Object.create(σ1);
    σ2[a] = v;
    return [ σ2, {type: 'address', a} ];
  },

  deref: function(σ, θ, {e}) {
    let [σ1, v] = ↆ(σ, θ, e);
    return [ σ1, deref_rules[v.type](σ1, v) ];
  },

  assign: function(σ, θ, {e1, e2}) {
    let [σ1, v1] = ↆ(σ, θ, e1);
    let [σ2, v2] = ↆ(σ1, θ, e2);
    return [ assign_rules[v1.type](σ2, v1, v2), v2 ];
  },
};

function interpretProgram(AST, env, store) {
  let env = env || {};
  let store = store || {};
  return interpretNode(env, store, AST);
}

// Test
interpretProgram({
  type: 'app',
  e1: {type: 'fun', x: 'x', e: {type: 'deref', e: {type: 'v', x: 'x'}}},
  e2: {type: 'ref', e: {type: 'c', c: 42}}
});
