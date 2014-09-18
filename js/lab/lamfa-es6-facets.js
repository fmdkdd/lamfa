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

  if (k > 0) {
    return {
      type: 'facet',
      k,
      vh: mk_facet(rest, v1, v2),
      vl: v2
    };
  }

  else {
    return {
      type: 'facet',
      k,
      vh: v2,
      vl: mk_facet(rest, v1, v2)
    };
  }
}

let application_rules = {
  bottom: function(σ) {
    return [σ, bottom];
  },

  closure: function(σ, pc, {x, e, θ}, v) {
    let θ1 = Object.create(θ);
    θ1[x] = v;
    return ↆ(σ, θ1, pc, e);
  },

  facet: function(σ, pc, {k, vh, vl}, v2) {
    if (pc.has(k)) {
      return application_rules(σ, pc, vh, v2);
    }

    else if (pc.has(-k)) {
      return application_rules(σ, pc, vl, v2);
    }

    else {
      let pck = new Set(pc);
      pck.add(k);
      let [σ1, vh1] = application_rules(σ, pck, vh, v2);
      let pcmk = new Set(pc);
      pcmk.add(-k);
      let [σ2, vl1] = application_rules(σ1, pcmk, vl, v2);
      return [ σ2, mk_facet(k, vh1, vl1) ];
    }
  }
};


let deref_rules = {
  bottom: function() {
    return bottom;
  },

  address: function(σ, {a}, pc) {
    return σ[a];
  },

  facet: function(σ, {k, vh, vl}, pc) {
    if (pc.has(k))
      return deref(σ, vh, pc);
    else if (pc.has(-k))
      return deref(σ, vl, pc);
    else
      return mk_facet(k, deref(σ, vh, pc), deref(σ, vl, pc));
  }
};

let assign_rules = {
  bottom: function(σ) {
    return σ;
  },

  address: function(σ, pc, {a}, v) {
    let σ2 = Object.create(σ);
    σ2[a] = mk_facet(pc, v, σ[a]);
    return σ2;
  },

  facet: function(σ, pc, {k, vh, vl}, v) {
    let pck = new Set(pc);
    pck.add(k);
    let σ1 = assign(σ, pck, vh, v);
    let pcmk = new Set(pc);
    pcmk.add(-k);
    return assign(σ1, pcmk, vl, v);
  }
};

let rules = {
  c: function(σ, θ, pc, {c}) {
    return [ σ, c ];
  },

  v: function(σ, θ, pc, {x}) {
    return [ σ, θ[x] ];
  },

  fun: function(σ, θ, pc, {x, e}) {
    return [ σ, {type: 'closure', x, e, θ} ];
  },

  app: function(σ, θ, pc, {e1, e2}) {
    let [σ1, v1] = ↆ(σ, θ, pc, e1);
    let [σ2, v2] = ↆ(σ1, θ, pc, e2);
    return application_rules[v1.type](σ2, pc, v1, v2);
  },

  ref: function(σ, θ, pc, {e}) {
    let [σ1, v] = ↆ(σ, θ, pc, e);
    let a = Object.keys(σ).length;
    let σ2 = Object.create(σ1);
    σ2[a] = mk_facet(pc, v, bottom);
    return [ σ2, {type: 'address', a} ];
  },

  deref: function(σ, θ, pc, {e}) {
    let [σ1, v] = ↆ(σ, θ, pc, e);
    return [ σ1, deref_rules[v.type](σ1, v, pc) ];
  },

  assign: function(σ, θ, pc, {e1, e2}) {
    let [σ1, v1] = ↆ(σ, θ, pc, e1);
    let [σ2, v2] = ↆ(σ1, θ, pc, e2);
    return [ assign_rules[v1.type](σ2, pc, v1, v2), v2 ];
  },
};

function interpretProgram(AST, env, store, pc) {
  let env = env || {};
  let store = store || {};
  let pc = new Set(pc || []);
  return interpretNode(env, store, pc, AST);
}

// Test
interpretProgram({
  type: 'app',
  e1: {type: 'fun', x: 'x', e: {type: 'deref', e: {type: 'v', x: 'x'}}},
  e2: {type: 'ref', e: {type: 'c', c: 42}}
}, {},{},[1]);
