var nodes = require('./parser').nodes;

var unparse = require('./unparser');

var σ = [];

function closure(λ, θ) {
  return {
    type: 'closure',
    λ: λ,
    θ: θ,
  };
}

function address(n) {
  return {
    type: 'address',
    value: n,
  };
}

var rules = {
  constant: function(node, θ) {
	  return node.value;
  },

  variable: function(node, θ) {
	  var value = θ[node.string];
    if (value === undefined)
      throw new InterpretError('Not in scope, ' + node.string);
    return value;
  },

  abstraction: function(node, θ) {
	  return closure(node, θ);
  },

  application: function(node, θ) {
    var closure = interpretNode(node.e1, θ);

    if (closure === '⟂')
      return '⟂';

    if (typeof closure !== 'object' || closure.type !== 'closure')
      throw new InterpretError('Not a closure, ' + closure);

    var v2 = interpretNode(node.e2, θ);
    var θ2 = Object.create(closure.θ);
    θ2[closure.λ.variable.string] = v2;
    return interpretNode(closure.λ.body, θ2);
  },

  reference: function(node, θ) {
    var v = interpretNode(node.expr, θ);
    var a = address(σ.length);
    σ[a.value] = v;
    return a;
  },

  dereference: function(node, θ) {
    var a = interpretNode(node.expr, θ);

    if (a === '⟂')
      return '⟂';

    if (typeof a !== 'object' || a.type !== 'address')
      throw new InterpretError('Not an address, ' + a);

    return σ[a.value];
  },

  assignment: function(node, θ) {
    var a = interpretNode(node.left, θ);
    var v = interpretNode(node.right, θ);
    σ[a.value] = v;
    return v;
  },

  '⟂': function(node, θ) {
	  return '⟂';
  },

  // read: function(node, θ) {

  // },

  // write: function(node, θ) {

  // },

  // bottom: function(node, θ) {

  // },
};

function interpretNode(node, θ) {
  debugger;
	var fn = rules[node.type];

	if (fn) return fn(node, θ);
	else throw new Error('Cannot interpret node: ' + JSON.stringify(node, null, 2));
}

function interpretProgram(AST, substitution) {
  substitution = substitution || {};
  return interpretNode(AST, substitution);
}

module.exports = interpretProgram;

function InterpretError(message) {
  this.message = message;
  this.name = "InterpretError";
}
