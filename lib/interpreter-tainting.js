var std = require('./interpreter-standard');

var result = std._result;
var rules = Object.create(std._rules); // Prevent accidental overwriting of our rules

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Tainting

var Address = require('./address');
var Closure = require('./closure');
var Store = require('./store');
var InterpretError = require('./error').InterpretError;

var Set = require('./set');
var Principal = require('./principal');
var TaintedValue = require('./taintedValue');

// Constant, Variable, Abtsraction are unaffected by taint

rules.application = function(node, θ, σ) {
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

rules.dereference = function(node, θ, σ) {
  var r = interpretNode(node.expr, θ, σ);

  var tainted = r.v instanceof TaintedValue;
  var a = tainted ? r.v.value : r.v;

  if (!(a instanceof Address))
    throw new InterpretError('Not an address, ' + a);

  var v = tainted ? new TaintedValue(r.σ.get(a)) : r.σ.get(a);

	return result(v, r.σ);
};

rules.assignment = function(node, θ, σ) {
  var r1 = interpretNode(node.left, θ, σ);
  var r2 = interpretNode(node.right, θ, r1.σ);
  var v = r2.v;

  var tainted = r1.v instanceof TaintedValue;
  var a = tainted ? r1.v.value : r1.v;

  if (!(a instanceof Address))
    throw new InterpretError('Not an address, ' + a);

  return result(v, r2.σ.set(a, v));
};

rules.taintExpr = function(node, θ, σ) {
  var r = interpretNode(node.value, θ, σ);

  return result(new TaintedValue(r.v), r.σ);
};

function interpretNode(node, θ, σ) {
	var fn = rules[node.type];

	if (fn) return fn(node, θ, σ);
	else throw new Error('Cannot interpret node: ' + JSON.stringify(node, null, 2));
}

function interpretProgram(AST, options) {
  options = options || {};
  var env = options.env || {};
  var store = new Store(options.store);
  var result = interpretNode(AST, env, store);
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
  _rules: Object.create(rules),
};
