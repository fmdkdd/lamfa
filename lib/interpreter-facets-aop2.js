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
var facetSubRules = {};

facetSubRules.dereference = {
  Facet: function(r, node, θ, σ, pc) {
    function deref(σ, v, pc) {
      var r = result(v, σ);
	    var res = std._subRule(v, std._dereferenceRules)(r, node, θ, σ, pc);
      return res.v;
    }

	  if (pc.has(r.v.k))
      var fv = deref(r.σ, r.v.vh, pc);
    if (pc.has(r.v.k.negation))
      var fv = deref(r.σ, r.v.vl, pc);
    else
      var fv = createFacet(r.v.k, deref(r.σ, r.v.vh, pc), deref(r.σ, r.v.vl, pc));

    return result(fv, r.σ);
  }
};

facetSubRules.assignment = {
  Address: function(r1, r2, node, θ, σ, pc) {
	  return result(r2.v, r2.σ.set(r1.v, createFacet(pc, r2.v, r2.σ.get(r1.v))));
  },

  Facet: function(r1, r2, node, θ, σ, pc) {
	  function assign(σ, pc, v1, v2) {
      var r1 = result(v1, σ);
      var r2 = result(v2, σ);
	    var res = std._subRule(r1.v, std._assignmentRules)(r1, r2, node, θ, σ, pc);
      return res.σ;
    }

    var σ1 = assign(r2.σ, pc.add(r1.v.k), r1.v.vh, r2.v);
    var σ2 = assign(σ1, pc.add(r1.v.k.negation), r1.v.vl, r2.v);
    return result(r2.v, σ2);
  },
};

facetSubRules.application = {
  Facet: function(r, node, θ, σ, pc) {
    function apply(v1, v2, θ, σ, pc) {
      var r1 = result(v1, σ);
      var r2 = result(v2, σ);
      return std._subRule(r1.v, std._applicationRules)(r1, node, θ, σ, pc);
    }

    var r1 = r;
    var r2 = interpretNode(node.e2, θ, r1.σ, pc);

    if (pc.has(r1.v.k)) {       // FA-LEFT
      return apply(r1.v.vh, r2.v, θ, σ, pc);
    }

    if (pc.has(r1.v.k.negation)) { // FA-RIGHT
      return apply(r1.v.vl, r2.v, θ, σ, pc);
    }

    else {                    // FA-SPLIT
      var pc1 = pc.add(r1.v.k);
      var rr1 = apply(r1.v.vh, r2.v, θ, σ, pc1);

      var pc2 = pc.add(r1.v.k.negation);
      var rr2 = apply(r1.v.vl, r2.v, θ, rr1.σ, pc2);

      return result(createFacet(r1.v.k, rr1.v, rr2.v), rr2.σ);
    }
  },
};

facetRules.reference = function(node, θ, σ, pc) {
  var r = interpretNode(node.expr, θ, σ, pc);
  var a = new Address(r.σ.size);
  var v = createFacet(pc, r.v, bottom);
  return result(a, r.σ.set(a, v));
};

// FIXME: crutch.  We need to define facetRules.dereference with a pc
// arg to pass it along to subRules, and we have to copy the body of
// the original dereference since we can't pass the PC more than one
// level deep.

facetRules.application = function(node, θ, σ, pc) {
    var r1 = interpretNode(node.e1, θ, σ, pc);

    return std._subRule(r1.v, std._applicationRules)(r1, node, θ, σ, pc);
};

facetRules.dereference = function(node, θ, σ, pc) {
  var r = interpretNode(node.expr, θ, σ, pc);
  return std._subRule(r.v, std._dereferenceRules)(r, node, θ, σ, pc);
}

facetRules.assignment = function(node, θ, σ, pc) {
  var r1 = interpretNode(node.left, θ, σ, pc);
  var r2 = interpretNode(node.right, θ, r1.σ, pc);
  return std._subRule(r1.v, std._assignmentRules)(r1, r2, node, θ, σ, pc);
}

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
function passPC(rules) {
  function passPCOnRule(rule) {
	  return function() {
      var args = [].slice.call(arguments);
      return rule.apply(this, args.concat(globalPC));
    }
  }

  Object.keys(rules).forEach(function(r) {
    rules[r] = passPCOnRule(rules[r]);
  });
}

passPC(facetRules);

passPC(facetSubRules.dereference);

// Save the original rules
var originalRules = _.extend({}, std._rules);

var originalDereferenceRules = _.extend({}, std._dereferenceRules);
var originalAssignmentRules = _.extend({}, std._assignmentRules);
var originalApplicationRules = _.extend({}, std._applicationRules);

function activateFacet() {
  // Overwrite original rules with facetRules
  _.extend(std._rules, facetRules);
  _.extend(std._dereferenceRules, facetSubRules.dereference);
  _.extend(std._assignmentRules, facetSubRules.assignment);
  _.extend(std._applicationRules, facetSubRules.application);
}

function deactivateFacet() {
  // Restore original rules
  _.extend(std._rules, originalRules);
  _.extend(std._dereferenceRules, originalDereferenceRules);
  delete std._dereferenceRules.Facet;

  _.extend(std._assignmentRules, originalAssignmentRules);
  delete std._assignmentRules.Facet;

  _.extend(std._applicationRules, originalApplicationRules);
  delete std._applicationRules.Facet;
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
