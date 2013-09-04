var nodes = require('./parser').nodes;
var Closure = require('./closure');
var Store = require('./store');
var Address = require('./address');

var unparse = require('./unparser');

var rules = {
  constant: function(node, θ, σ) {
	  return {
      v: node.value,
      σ: σ,
    };
  },

  variable: function(node, θ, σ) {
    if (!(node.string in θ))
      throw new InterpretError('Not in scope, ' + node.string);

    return {
      v: θ[node.string],
      σ: σ,
    };
  },

  abstraction: function(node, θ, σ) {
	  return {
      v: new Closure(node, θ),
      σ: σ,
    }
  },

  application: function(node, θ, σ) {
    var r1 = interpretNode(node.e1, θ, σ);
    var closure = r1.v;

    if (closure === '⟂')
      return {
        v: '⟂',
        σ: σ,
      }

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
    return {
      v: a,
      σ: r.σ.set(a, r.v),
    };
  },

  dereference: function(node, θ, σ) {
    var r = interpretNode(node.expr, θ, σ);
    var a = r.v;

    if (a === '⟂')
	    return {
        v: '⟂',
        σ: r.σ,
      };

    if (!(a instanceof Address))
      throw new InterpretError('Not an address, ' + a);

	  return {
      v: r.σ.get(a),
      σ: r.σ,
    };
  },

  assignment: function(node, θ, σ) {
    var r1 = interpretNode(node.left, θ, σ);
    var a = r1.v;
    var r2 = interpretNode(node.right, θ, r1.σ);
    var v = r2.v;

    if (a === '⟂')              // S-ASSIGN-BOT
      return {
        v: v,
        σ: r2.σ,
      };

    if (!(a instanceof Address))
      throw new InterpretError('Not an address, ' + a);

    return {                    // S-ASSIGN
      v: v,
      σ: r2.σ.set(a, v),
    };
  },

  '⟂': function(node, θ, σ) {
	  return {
      v: '⟂',
      σ: σ,
    };
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
  return interpretNode(AST, substitution, store);
}

module.exports = {
  interpret: interpretProgram,
  Closure: Closure,
  Address: Address,
}

function InterpretError(message) {
  this.message = message;
  this.name = "InterpretError";
}
