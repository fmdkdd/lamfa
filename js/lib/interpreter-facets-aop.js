var std = require('./interpreter-standard');

var result = std._result;
var rules = Object.create(std._rules); // Prevent accidental overwriting of our rules

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Facets using aspects

var Address = require('./address');
var Bottom = require('./bottom');
var Closure = require('./closure');
var Store = require('./store');
var InterpretError = require('./error').InterpretError;

var Set = require('./set');
var Principal = require('./principal');

var facets = require('./facet');
var createFacet = facets.createFacet;
var Facet = facets.Facet;

var bottom = new Bottom();

// Constant, Variable, Abtsraction are unaffected by facets

rules.application = function(node, θ, σ, pc) {
  function interpretApplication(v1, v2, θ, σ, pc) {
    if (v1 instanceof Bottom)   // FA-BOT
      return result(bottom, σ);

    if (v1 instanceof Closure) { // FA-FUN
      var θ2 = Object.create(v1.θ);
      θ2[v1.λ.variable.string] = v2;
      return interpretNode(v1.λ.body, θ2, σ, pc);
    }

    if (v1 instanceof Facet) {
      if (pc.has(v1.k)) {       // FA-LEFT
        return interpretApplication(v1.vh, v2, θ, σ, pc);
      }

      if (pc.has(v1.k.negation)) { // FA-RIGHT
        return interpretApplication(v1.vl, v2, θ, σ, pc);
      }

      else {                    // FA-SPLIT
        var pc1 = pc.add(v1.k);
        var r1 = interpretApplication(v1.vh, v2, θ, σ, pc1);

        var pc2 = pc.add(v1.k.negation);
        var r2 = interpretApplication(v1.vl, v2, θ, r1.σ, pc2);

        return result(createFacet(v1.k, r1.v, r2.v), r2.σ);
      }
    }

    throw new InterpretError('Expected Bottom, Closure, Facet but got ' + JSON.stringify(v1, null, 2));
  }

  var r1 = interpretNode(node.e1, θ, σ, pc);
  var r2 = interpretNode(node.e2, θ, r1.σ, pc);
  return interpretApplication(r1.v, r2.v, θ, r2.σ, pc);
};

rules.reference = function(node, θ, σ, pc) {
  var r = interpretNode(node.expr, θ, σ, pc);
  var a = new Address(r.σ.size);
  var v = createFacet(pc, r.v, bottom);
  return result(a, r.σ.set(a, v));
}

rules.dereference = function(node, θ, σ, pc) {
  function deref(σ, v, pc) {
    if (v instanceof Bottom)
      return bottom;

    if (v instanceof Address)
      return σ.get(v);

    if (v instanceof Facet) {
      if (pc.has(v.k))
        return deref(σ, v.vh, pc);
      if (pc.has(v.k.negation))
        return deref(σ, v.vl, pc);
      else
        return createFacet(v.k, deref(σ, v.vh, pc), deref(σ, v.vl, pc));
    }

    throw new InterpretError('Expected Bottom, Address, Facet but got ' + JSON.stringify(v, null, 2));
  }

  var r = interpretNode(node.expr, θ, σ, pc);
  var v = deref(r.σ, r.v, pc);

  return result(v, r.σ);
};

rules.assignment = function(node, θ, σ, pc) {
  function assign(σ, pc, v1, v2) {
    if (v1 instanceof Bottom)
      return σ;

    if (v1 instanceof Address) {
      return σ.set(v1, createFacet(pc, v2, σ.get(v1)));
    }

    if (v1 instanceof Facet) {
      var σ1 = assign(σ, pc.add(v1.k), v1.vh, v2);
      return assign(σ1, pc.add(v1.k.negation), v1.vl, v2);
    }

    throw new InterpretError('Expected Bottom, Address, Facet but got ' + v1);
  }

  var r1 = interpretNode(node.left, θ, σ, pc);
  var r2 = interpretNode(node.right, θ, r1.σ, pc);

  return result(r2.v, assign(r2.σ, pc, r1.v, r2.v));
};

rules.facetExpr = function(node, θ, σ, pc) {
  var k = new Principal(node.principal.value);

  if (pc.has(k)) {
    // F-LEFT
    return interpretNode(node.high, θ, σ, pc);
  }

  if (pc.has(k.negation)) {
    // F-RIGHT
    return interpretNode(node.low, θ, σ, pc);
  }

  // F-SPLIT
  var pc1 = pc.add(k);
  var r1 = interpretNode(node.high, θ, σ, pc1);

  var pc2 = pc.add(k.negation);
  var r2 = interpretNode(node.low, θ, r1.σ, pc2);

  return result(createFacet(k, r1.v, r2.v), r2.σ);
};

// Close over our extended rules object
function interpretNode(node, θ, σ, pc) {
	var fn = rules[node.type];

	if (fn) return fn(node, θ, σ, pc);
	else throw new Error('Cannot interpret node: ' + JSON.stringify(node, null, 2));
}

// Close over our interpretNode function
function interpretProgram(AST, options) {
  options = options || {};
  var env = options.env || {};
  var store = new Store(options.store);
  options.pc = options.pc || [];
  var pc = new Set(options.pc.map(function(label) { return new Principal(label); }));
  var result = interpretNode(AST, env, store, pc);
  return {
    value: result.v,
    store: result.σ,
  };
}

module.exports = {
  // User exports
  interpret: interpretProgram,

  // Extender exports
  _interpretProgram: interpretProgram,
  _interpretNode: interpretNode,
  _rules: rules,
};
