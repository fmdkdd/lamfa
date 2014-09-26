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
    eval_apply(C, v2) {
      if (C.pc.has(k)) {
        return vh.eval_apply(C, v2);
      }

      else if (pc.has(-k)) {
        return vl.eval_apply(C, v2);
      }

      else {
        let [C1, vh1] = vh.eval_apply(with_pc(C, C.pc.union(k)), v2);
        let [C2, vl1] = vl.eval_apply(with_pc(C1, C.pc.union(-k)), v2);
        return [ C2, mk_facet(k, vh1, vl1) ];
      }
    },

    eval_deref(C) {
      if (pc.has(k))
        return vh.eval_deref(C);
      else if (pc.has(-k))
        return vl.eval_deref(C);
      else
        return mk_facet(k, vh.eval_deref(C), vl.eval_deref(C));
    },

    eval_assign(C, v) {
      let C1 = vh.eval_assign(with_pc(C, C.pc.union(k)), v);
      let C2 = vl.eval_assign(with_pc(C1, C.pc.union(-k)), v);
      return C2;
    }
  };
}

function with_pc(C, pc) {
  return {__proto__: C, pc};
}

function interpretFacetProgram(AST, env = {}, store = {}, pc = []) {
  return AST.eval({σ: env, θ: store, pc: new Set(pc)});
}