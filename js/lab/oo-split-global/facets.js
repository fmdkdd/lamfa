Set.prototype.union = function(elem) {
  let n = new Set(this);
  n.add(elem);
  return n;
};

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
    k, vh, vl,
    eval_apply(σ, v2) {
      if (pc.has(k)) {
        return vh.eval_apply(σ, v2);
      }

      else if (pc.has(-k)) {
        return vl.eval_apply(σ, v2);
      }

      else {
        let [σ1, vh1] = with_pc(pc.union(k), () => vh.eval_apply(σ, v2));
        let [σ2, vl1] = with_pc(pc.union(-k), () => vl.eval_apply(σ1, v2));
        return [ σ2, mk_facet(k, vh1, vl1) ];
      }
    },

    eval_deref(σ) {
      if (pc.has(k))
        return vh.eval_deref(σ);
      else if (pc.has(-k))
        return vl.eval_deref(σ);
      else
        return mk_facet(k, vh.eval_deref(σ), vl.eval_deref(σ));
    },

    eval_assign(σ, v) {
      let σ1 = with_pc(pc.union(k), () => vh.eval_assign(σ, v));
      let σ2 = with_pc(pc.union(-k), () => vl.eval_assign(σ1, v));
      return σ2;
    }
  };
}

function with_pc(new_pc, thunk) {
  let old_pc = pc;
  pc = new_pc;
  let ret = thunk();
  pc = old_pc;
  return ret;
}

let pc = null;

function interpretFacetProgram(AST, env = {}, store = {}, default_pc = []) {
  return with_pc(new Set(default_pc), () =>
                 interpretProgram(AST, env, store));
}
