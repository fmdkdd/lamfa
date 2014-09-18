function interpretNode(σ, θ, node) {
  for (var r in rules) {
    try {
      return rules[r](σ, θ, node);
    } catch (e) {
      // console.log('failed match for rule', r);
    }
  }

  // console.log('non exhaustive pattern matching');
}

let ↆ = interpretNode;

let rules = {
  constant: function(σ, θ, {c: [c]}) {
    return [ σ, c ];
  },

  variable: function(σ, θ, {x: [x]}) {
    return [ σ, θ[x] ];
  },

  abstraction: function(σ, θ, {λ: [x, e]}) {
    return [ σ, [x, e, θ] ];
  },

  application: function(σ, θ, {apply: [e1, e2]}) {
    let [σ1, [x, e, θ1]] = ↆ(σ, θ, e1);
    let [σ2, v1] = ↆ(σ1, θ, e2);
    let θ2 = Object.create(θ1);
    θ2[x] = v1;
    return ↆ(σ, θ2, e);
  },

  reference: function(σ, θ, {ref: [e]}) {
    let [σ1, v] = ↆ(σ, θ, e);
    let a = Object.keys(σ).length;
    let σ2 = Object.create(σ1);
    σ2[a] = r.v;
    return [ σ2, a ];
  },

  dereference: function(σ, θ, {deref: [e]}) {
    let [σ1, a] = ↆ(σ, θ, e);
    return [ σ1, σ1[a] ];
  },

  assignment: function(σ, θ, {assign: [e1, e2]}) {
    let [σ1, a] = ↆ(σ, θ, e1);
    let [σ2, v] = ↆ(σ1, θ, e2);
    let σ3 = Object.create(σ2);
    σ3[a] = v;
    return [ σ3, v ];
  },
};

function interpretProgram(AST, env, store) {
  let env = env || {};
  let store = store || {};
  return interpretNode(env, store, AST);
}

// Test
interpretNode({}, {}, {
  apply: [
    {λ: ['x', {x: ['x']}]},
    {c: [42]}
  ]
});
