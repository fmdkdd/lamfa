var std = require('./interpreter-standard');

var _ = {
  extend: function(obj, ext) {
    for (var prop in ext)
      obj[prop] = ext[prop];
    return obj;
  }
};

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Facets using aspects

var Address = require('./address');
var Bottom = require('./bottom');
var Closure = require('./closure');
var InterpretError = require('./error').InterpretError;

var Set = require('./set');
var Principal = require('./principal');

var facets = require('./facet');
var createFacet = facets.createFacet;
var Facet = facets.Facet;

var bottom = new Bottom();

// Constant, Variable, Abtsraction are unaffected by facets

var result = std._result;
var facetRules = {};

// How to reduce this further: maybe rewrite the rules themselves in
// pretty-big-step semantics ?

facetRules.application = function(node, θ, σ, pc) {
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
}

facetRules.reference = function(node, θ, σ, pc) {
  var r = interpretNode(node.expr, θ, σ, pc);
  var a = new Address(r.σ.size);
  var v = createFacet(pc, r.v, bottom);
  return result(a, r.σ.set(a, v));
}

facetRules.dereference = function(node, θ, σ, pc) {
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

facetRules.assignment = function(node, θ, σ, pc) {
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

facetRules.facetExpr = function(node, θ, σ, pc) {
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

// Bring the pc along all the calls to rules

// toString works, but is brittle, not supported everywhere, and
// discards the original closure.
//eval(std._interpretNode.toString().replace(/\(node, θ, σ\)/g, '(node, θ, σ, pc)'));

// eval(std._interpretProgram.toString().replace('var result = interpretNode(AST, env, store);',
//                                               'options.pc = options.pc || [];'
//                                               + 'var pc = new Set(options.pc.map(function(label) { return new Principal(label); }));'
//                                               + 'var result = interpretNode(AST, env, store, pc);'));

// The better solution would be to put the `pc` in an aspect

// Scoping strategies, Tanter & Distributed Scoping Strategies
// AspectJ.  This should follow the flow, to avoid funkyness in
// interleaved execution.
var globalPC;

// Update pc on calls to interpretNode
function interpretNode(node, θ, σ, pc) {
  globalPC = pc;
	return std._interpretNode(node, θ, σ);
}

// Set pc on call to interpretProgram
function interpretProgram(AST, options) {
  options = options || {};
  options.pc = options.pc || [];
  globalPC = new Set(options.pc.map(function(label) { return new Principal(label); }));
  return std._interpretProgram(AST, options);
}

// Pass pc along on calls to the facet rules
function passPC(rule) {
	return function(node, θ, σ) {
    return rule(node, θ, σ, globalPC);
  }
}

Object.keys(facetRules).forEach(function(r) {
  facetRules[r] = passPC(facetRules[r]);
});

// Save the original rules
var originalRules = _.extend({}, std._rules);

function activateFacet() {
  // Overwrite original rules with facetRules
  _.extend(std._rules, facetRules);
}

function deactivateFacet() {
  // Restore original rules
  _.extend(std._rules, originalRules);
}

module.exports = {
  // User exports
  interpret: interpretProgram,

  activateFacet: activateFacet,
  deactivateFacet: deactivateFacet,

 // Extender exports
  _interpretProgram: interpretProgram,
  _interpretNode: interpretNode,
  _facetRules: facetRules,
};
