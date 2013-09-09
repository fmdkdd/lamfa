var nodes = require('./parser').nodes;
var Set = require('./set');
var Closure = require('./closure');
var Store = require('./store');
var Address = require('./address');

var bottom = '⟂';

function Facet(k, vh, vl) {
  this.k = k;
  this.vh = vh;
  this.vl = vl;
}

function createFacet(k, vh, vl) {
  if (typeof k == 'number')
    k = new Principal(k);

  if (k instanceof Principal)
    k = new Set([k]);

  if (!k instanceof Set)
    throw new InterpretError('Not a set, ' + k);

  if (k.isEmpty())
    return vh;

  var l = k.first;
  var rest = k.rest;

  if (l.isNegated())
    return new Facet(l.negation, vl, createFacet(rest, vh, vl));
  else
    return new Facet(l, createFacet(rest, vh, vl), vl);
}

function Principal(label, negate) {
  if (typeof negate === 'undefined') {
    this.label = Math.abs(label);
    this.negated = label < 0;
  } else {
    this.label = label;
    this.negated = negate || false;
  }
}

Object.defineProperty(Principal.prototype, 'negation', {
  get: function() {
	  return new Principal(this.label, !this.isNegated());
  }
});

Principal.prototype.isNegated = function() {
	return !!this.negated;
};

function result(value, σ) {
	return {
    v: value,
    σ: σ,
  };
}

var rules = {
  constant: function(node, θ, σ, pc) {
	  return result(node.value, σ);
  },

  variable: function(node, θ, σ, pc) {
    if (!(node.string in θ))
      throw new InterpretError('Not in scope, ' + node.string);

    return result(θ[node.string], σ);
  },

  abstraction: function(node, θ, σ, pc) {
	  return result(new Closure(node, θ), σ);
  },

  application: function(node, θ, σ, pc) {
    var r1 = interpretNode(node.e1, θ, σ, pc);
    var r2 = interpretNode(node.e2, θ, r1.σ, pc);
    return interpretApplication(r1.v, r2.v, θ, r2.σ, pc);
  },

  reference: function(node, θ, σ, pc) {
    var r = interpretNode(node.expr, θ, σ, pc);
    var a = new Address(r.σ.size);
    var v = createFacet(pc, r.v, bottom);
    return result(a, r.σ.set(a, v));
  },

  dereference: function(node, θ, σ, pc) {
    function deref(σ, v, pc) {
      if (v === bottom)
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

      throw new InterpretError('Not an address or facet, ' + JSON.stringify(v, null, 2));
    }

    var r = interpretNode(node.expr, θ, σ, pc);
    var v = deref(r.σ, r.v, pc);

    return result(v, r.σ);
  },

  assignment: function(node, θ, σ, pc) {
    function assign(σ, pc, v1, v2) {
      if (v1 === bottom)
        return σ;

      if (v1 instanceof Address) {
        return σ.set(v1, createFacet(pc, v2, σ.get(v1)));
      }

      if (v1 instanceof Facet) {
        var σ1 = assign(σ, pc.add(v1.k), v1.vh, v2);
        return assign(σ1, pc.add(v1.k.negation), v1.vl, v2);
      }

      throw new InterpretError('Not an address or facet, ' + v1);
    }

    var r1 = interpretNode(node.left, θ, σ, pc);
    var r2 = interpretNode(node.right, θ, r1.σ, pc);

    return result(r2.v, assign(r2.σ, pc, r1.v, r2.v));
  },

  '⟂': function(node, θ, σ, pc) {
	  return result('⟂', σ);
  },

  facetExpr: function(node, θ, σ, pc) {
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
  },

  // read: function(node, θ) {

  // },

  // write: function(node, θ) {

  // },
};

function interpretApplication(v1, v2, θ, σ, pc) {
  if (v1 === bottom)            // FA-BOT
    return result(bottom, σ);

  if (v1 instanceof Closure) {  // FA-FUN
    var θ2 = Object.create(v1.θ);
    θ2[v1.λ.variable.string] = v2;
    return interpretNode(v1.λ.body, θ2, σ, pc);
  }

  if (v1 instanceof Facet) {
    if (pc.has(v1.k)) {         // FA-LEFT
      return interpretApplication(v1.vh, v2, θ, σ, pc);
    }

    if (pc.has(v1.k.negation)) { // FA-RIGHT
      return interpretApplication(v1.vl, v2, θ, σ, pc);
    }

    else {                      // FA-SPLIT
      var pc1 = pc.add(v1.k);
      var r1 = interpretApplication(v1.vh, v2, θ, σ, pc1);

      var pc2 = pc.add(v1.k.negation);
      var r2 = interpretApplication(v1.vl, v2, θ, r1.σ, pc2);

      return result(createFacet(v1.k, r1.v, r2.v), r2.σ);
    }
  }

  throw new InterpretError('Not a closure or facet, ' + JSON.stringify(v1, null, 2));
}

function interpretNode(node, θ, σ, pc) {
	var fn = rules[node.type];

	if (fn) return fn(node, θ, σ, pc);
	else throw new Error('Cannot interpret node: ' + JSON.stringify(node, null, 2));
}

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

var unparse = require('./unparser');

function prettyPrint(value) {
  if (value instanceof Address)
    return '#' + value.value;

  if (value instanceof Closure) {
    var lambda = unparse(value.λ);
    if (lambda === '(λx.(λy.x))')
      lambda = true;
    if (lambda === '(λx.(λy.y))')
      lambda = false;

    return lambda;
  }

  if (value instanceof Facet) {
    if (value.k.negated)
      return '<' + value.k.label + ' ? ' + prettyPrint(value.vl) + ' : ' + prettyPrint(value.vh) + '>';
    else
      return '<' + value.k.label + ' ? ' + prettyPrint(value.vh) + ' : ' + prettyPrint(value.vl) + '>';
  }

  return value;
}

module.exports = {
  interpret: interpretProgram,
  Closure: Closure,
  Address: Address,
  createFacet: createFacet,
  prettyPrint: prettyPrint,
};

function InterpretError(message) {
  this.message = message;
  this.name = "InterpretError";
}
