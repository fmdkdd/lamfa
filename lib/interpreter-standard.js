var nodes = require('./parser').nodes;
var Closure = require('./closure');
var Store = require('./store');
var Address = require('./address');
var InterpretError = require('./error').InterpretError;

function result(value, σ) {
	return {
    v: value,
    σ: σ,
  };
}

var rules = {
  constant: function(node, θ, σ) {
	  return result(node.value, σ);
  },

  variable: function(node, θ, σ) {
    if (!(node.string in θ))
      throw new InterpretError('Not in scope, ' + node.string);

    return result(θ[node.string], σ);
  },

  abstraction: function(node, θ, σ) {
	  return result(new Closure(node, θ), σ);
  },

  application: function(node, θ, σ) {
    var r1 = interpretNode(node.e1, θ, σ);
    var closure = r1.v;

    if (closure === '⟂')
      return result('⟂', σ);

    if (!(closure instanceof Closure))
      throw new InterpretError('Not a closure, ' + closure);

    var r2 = interpretNode(node.e2, θ, r1.σ);
    var v2 = r2.v;
    var θ2 = Object.create(closure.θ);
    θ2[closure.λ.variable.string] = v2;
    return interpretNode(closure.λ.body, θ2, σ);
  },

  reference: function(node, θ, σ) {
    var r = interpretNode(node.expr, θ, σ);
    var a = new Address(r.σ.size);
    return result(a, r.σ.set(a, r.v));
  },

  dereference: function(node, θ, σ) {
    var r = interpretNode(node.expr, θ, σ);
    var a = r.v;

    if (a === '⟂')
	    return result('⟂', r.σ);

    if (!(a instanceof Address))
      throw new InterpretError('Not an address, ' + a);

	  return result(r.σ.get(a), r.σ);
  },

  assignment: function(node, θ, σ) {
    var r1 = interpretNode(node.left, θ, σ);
    var a = r1.v;
    var r2 = interpretNode(node.right, θ, r1.σ);
    var v = r2.v;

    if (a === '⟂')              // S-ASSIGN-BOT
      return result(v, r2.σ);

    if (!(a instanceof Address))
      throw new InterpretError('Not an address, ' + a);

    return result(v, r2.σ.set(a, v)); // S-ASSIGN
  },

  '⟂': function(node, θ, σ) {
	  return result('⟂', σ);
  },

  // read: function(node, θ) {

  // },

  // write: function(node, θ) {

  // },
};

function interpretNode(node, θ, σ) {
	var fn = rules[node.type];

	if (fn) return fn(node, θ, σ);
	else throw new Error('Cannot interpret node: ' + JSON.stringify(node, null, 2));
}

function interpretProgram(AST, substitution, storeElements) {
  substitution = substitution || {};
  var store = new Store(storeElements);
  var result = interpretNode(AST, substitution, store);
  return {
    value: result.v,
    store: result.σ,
  };
}

module.exports = interpretProgram;
