var std = require('./interpreter-standard');

var _ = {
  extend: function(obj, ext) {
    for (var prop in ext)
      obj[prop] = ext[prop];
    return obj;
  }
};

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Tainting

var Address = require('./address');
var Bottom = require('./bottom');
var Closure = require('./closure');
var Store = require('./store');
var InterpretError = require('./error').InterpretError;

var TaintedValue = require('./taintedValue');

var result = std._result;
var taintingRules = {};

// Constant, Variable, Abstraction are unaffected by taint

taintingRules.application = function(node, θ, σ) {
  var r1 = interpretNode(node.e1, θ, σ);

  var tainted = r1.v instanceof TaintedValue;

  var closure = tainted ? r1.v.value : r1.v;

  if (!(closure instanceof Closure))
    throw new InterpretError('Not a closure or tainted value, ' + closure);

  var r2 = interpretNode(node.e2, θ, r1.σ);
  var v2 = r2.v;
  var θ2 = Object.create(closure.θ);
  θ2[closure.λ.variable.string] = v2;
  var r = interpretNode(closure.λ.body, θ2, r2.σ);
  var v = tainted ? new TaintedValue(r.v) : r.v;
  return result(v, r.σ);
};

taintingRules.dereference = function(node, θ, σ) {
  var r = interpretNode(node.expr, θ, σ);

  var tainted = r.v instanceof TaintedValue;
  var a = tainted ? r.v.value : r.v;

  if (!(a instanceof Address))
    throw new InterpretError('Not an address, ' + a);

  var v = tainted ? new TaintedValue(r.σ.get(a)) : r.σ.get(a);

	return result(v, r.σ);
};

taintingRules.assignment = function(node, θ, σ) {
  var r1 = interpretNode(node.left, θ, σ);
  var r2 = interpretNode(node.right, θ, r1.σ);
  var v = r2.v;

  var tainted = r1.v instanceof TaintedValue;
  var a = tainted ? r1.v.value : r1.v;

  if (!(a instanceof Address))
    throw new InterpretError('Not an address, ' + a);

  return result(v, r2.σ.set(a, v));
};

taintingRules.taintExpr = function(node, θ, σ) {
  var r = interpretNode(node.value, θ, σ);

  return result(new TaintedValue(r.v), r.σ);
};

var interpretNode = std._interpretNode;
var interpretProgram = std._interpretProgram;

// Save the original rules
var originalRules = _.extend({}, std._rules);

function activateTainting() {
  // Overwrite original rules with facetRules
  _.extend(std._rules, taintingRules);
}

function deactivateTainting() {
  // Restore original rules
  _.extend(std._rules, originalRules);
}

module.exports = {
  // User exports
  interpret: interpretProgram,

  activateTainting: activateTainting,
  deactivateTainting: deactivateTainting,

  // Extender exports
  _interpretProgram: interpretProgram,
  _interpretNode: interpretNode,
  _taintingRules: taintingRules,
};
