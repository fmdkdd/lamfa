var std = require('./interpreter-standard');

var _ = {
  extend: function(obj, extension) {
    var erased = {},
    newProps = [];

	  for (var prop in extension) {
      if (prop in obj) erased[prop] = obj[prop];
      else newProps.push(prop);
      obj[prop] = extension[prop];
    }

    return function undo() {
	    for (var prop in erased) obj[prop] = erased[prop];
	    newProps.forEach(function delProp(prop) {
	      delete obj[prop];
      });
    };
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
  Facet: function(r, node, θ, σ) {
    function deref(σ, v) {
      var r = result(v, σ);
	    var res = std._subRule(v, std._dereferenceRules)(r, node, θ, σ);
      return res.v;
    }

	  if (globalPC.has(r.v.k))
      var fv = deref(r.σ, r.v.vh);
    if (globalPC.has(r.v.k.negation))
      var fv = deref(r.σ, r.v.vl);
    else
      var fv = createFacet(r.v.k, deref(r.σ, r.v.vh), deref(r.σ, r.v.vl));

    return result(fv, r.σ);
  }
};

facetSubRules.assignment = {
  Address: function(r1, r2, node, θ, σ) {
	  return result(r2.v, r2.σ.set(r1.v, createFacet(globalPC, r2.v, r2.σ.get(r1.v))));
  },

  Facet: function(r1, r2, node, θ, σ) {
	  function assign(σ, v1, v2) {
      var r1 = result(v1, σ);
      var r2 = result(v2, σ);
	    var res = std._subRule(r1.v, std._assignmentRules)(r1, r2, node, θ, σ);
      return res.σ;
    }

    var σ1 = withPC(globalPC.add(r1.v.k), function() {
       return assign(r2.σ, r1.v.vh, r2.v);
    });

    var σ2 = withPC(globalPC.add(r1.v.k.negation), function() {
	    return assign(σ1, r1.v.vl, r2.v);
    });

    return result(r2.v, σ2);
  },
};

facetSubRules.application = {
  Facet: function(r, node, θ, σ) {
    function apply(v1, v2, θ, σ) {
      var r1 = result(v1, σ);
      var r2 = result(v2, σ);
      return std._subRule(r1.v, std._applicationRules)(r1, node, θ, σ);
    }

    var r1 = r;
    var r2 = interpretNode(node.e2, θ, r1.σ);

    if (globalPC.has(r1.v.k)) {       // FA-LEFT
      return apply(r1.v.vh, r2.v, θ, σ);
    }

    if (globalPC.has(r1.v.k.negation)) { // FA-RIGHT
      return apply(r1.v.vl, r2.v, θ, σ);
    }

    else {                      // FA-SPLIT

      var rr1 = withPC(globalPC.add(r1.v.k), function() {
	      return apply(r1.v.vh, r2.v, θ, σ);
      });

      var rr2 = withPC(globalPC.add(r1.v.k.negation), function() {
	      return apply(r1.v.vl, r2.v, θ, rr1.σ);
      });

      return result(createFacet(r1.v.k, rr1.v, rr2.v), rr2.σ);
    }
  },
};

facetRules.reference = function(node, θ, σ) {
  var r = interpretNode(node.expr, θ, σ);
  var a = new Address(r.σ.size);
  var v = createFacet(globalPC, r.v, bottom);
  return result(a, r.σ.set(a, v));
};

facetRules.facetExpr = function(node, θ, σ) {
  var k = new Principal(node.principal.value);

  if (globalPC.has(k)) {
    // F-LEFT
    return interpretNode(node.high, θ, σ);
  }

  if (globalPC.has(k.negation)) {
    // F-RIGHT
    return interpretNode(node.low, θ, σ);
  }

  // F-SPLIT

  var r1 = withPC(globalPC.add(k), function() {
    return interpretNode(node.high, θ, σ);
  })

  var r2 = withPC(globalPC.add(k.negation), function() {
	  return interpretNode(node.low, θ, r1.σ);
  });

  return result(createFacet(k, r1.v, r2.v), r2.σ);
};

// Bring the pc along all the calls to rules
var globalPC;

// For temporary PC substitutions
function withPC(value, thunk) {
	var original = globalPC;
  globalPC = value;
  var ret = thunk();
  globalPC = original;
  return ret;
}

var interpretNode = std._interpretNode;

// Set global pc on call to interpretProgram
function interpretProgram(AST, options) {
  options = options || {};
  options.pc = options.pc || [];
  globalPC = new Set(options.pc.map(function(label) { return new Principal(label); }));
  return std._interpretProgram(AST, options);
}

// Save the original rules
var id = function() {};

var restoreRules = id;
var restoreDereferenceRules = id;
var restoreAssignmentRules = id;
var restoreApplicationRules = id;

function activateFacet() {
  restoreRules = _.extend(std._rules, facetRules);
  restoreDereferenceRules = _.extend(std._dereferenceRules,
                                   facetSubRules.dereference);
  restoreAssignmentRules = _.extend(std._assignmentRules,
                                  facetSubRules.assignment);
  restoreApplicationRules = _.extend(std._applicationRules,
                                   facetSubRules.application);
}

function deactivateFacet() {
  // Restore original rules
  restoreRules();
  restoreDereferenceRules();
  restoreAssignmentRules();
  restoreApplicationRules();
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
