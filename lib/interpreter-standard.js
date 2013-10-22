var Map = require('./map');
var Address = require('./address');
var Bottom = require('./bottom');
var Closure = require('./closure');
var Store = require('./store');
var InterpretError = require('./error').InterpretError;

function result(value, σ) {
	return {
    v: value,
    σ: σ,
  };
}

function valueType(value, types) {
  types.first = function(predicate) {
    var result;
    this.some(function(val) {
      if (predicate(val)) {
        result = val;
        return true;
      }
      return false;
    });
    return result;
  };

  function isMatchingType(type) { return value instanceof type; }

  return types.first(isMatchingType);
}

function subRule(value, rules) {
  var typeNames = Object.keys(rules);
  var types = typeNames.map(function(n) { return eval(n); }); // from string to constructor
  var type = valueType(value, types);

  if (type === undefined) {
    throw new InterpretError('Expected ' + typeNames.join(', ') + ' but got ' + value);
  }

  return rules[type.name];
}


var bottom = new Bottom();

var applicationRules = Map({
  Bottom: function(r, node, θ, σ) {
    return result(bottom, σ);
  },

  Closure: function(r, node, θ, σ) {
    var closure = r.v;
    var r2 = interpretNode(node.e2, θ, r.σ);
    var v2 = r2.v;
    var θ2 = Object.create(closure.θ);
    θ2[closure.λ.variable.string] = v2;
    return interpretNode(closure.λ.body, θ2, r2.σ);
  },
});

var dereferenceRules = Map({
  Bottom: function(r, node, θ, σ) {
	  return result(bottom, r.σ);
  },

  Address: function(r, node, θ, σ) {
	  return result(r.σ.get(r.v), r.σ);
  },
});

var assignmentRules = Map({
  Bottom: function(r1, r2, node, θ, σ) {
	  return result(r2.v, r2.σ);
  },

  Address: function(r1, r2, node, θ, σ) {
	  return result(r2.v, r2.σ.set(r1.v, r2.v));
  },
});

var rules = Map({
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

    return subRule(r1.v, applicationRules)(r1, node, θ, σ);
  },

  reference: function(node, θ, σ) {
    var r = interpretNode(node.expr, θ, σ);
    var a = new Address(r.σ.size);
    return result(a, r.σ.set(a, r.v));
  },

  dereference: function(node, θ, σ) {
    var r = interpretNode(node.expr, θ, σ);
    return subRule(r.v, dereferenceRules)(r, node, θ, σ);
  },

  assignment: function(node, θ, σ) {
    var r1 = interpretNode(node.left, θ, σ);
    var r2 = interpretNode(node.right, θ, r1.σ);

    return subRule(r1.v, assignmentRules)(r1, r2, node, θ, σ);
  },

  '⟂': function(node, θ, σ) {
	  return result(bottom, σ);
  },

  // read: function(node, θ) {

  // },

  // write: function(node, θ) {

  // },
});

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
  // Users exports
  interpret: interpretProgram,

  // Extenders exports
  _interpretProgram: interpretProgram,
  _interpretNode: interpretNode,
  _rules: rules,
  _applicationRules: applicationRules,
  _dereferenceRules: dereferenceRules,
  _assignmentRules: assignmentRules,
  _result: result,
};
