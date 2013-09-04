var nodes = require('./parser').nodes;
var Set = require('./set');

var Σ = [];

var bottom = '⟂';

function Closure(λ, Θ) {
  this.λ = λ;
  this.Θ = Θ;
}

function Address(n) {
  this.value = n;
}

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
  this.label = label;
  this.negated = negate || false;
}

Object.defineProperty(Principal.prototype, 'negation', {
  get: function() {
	  return new Principal(this.label, !this.isNegated());
  }
});

Principal.prototype.isNegated = function() {
	return !!this.negated;
}

var rules = {
  constant: function(node, Θ, pc) {
	  return node.value;
  },

  variable: function(node, Θ, pc) {
	  var value = Θ[node.string];
    if (value === undefined)
      throw new InterpretError('Not in scope, ' + node.string);
    return value;
  },

  abstraction: function(node, Θ, pc) {
	  return new Closure(node, Θ);
  },

  application: function(node, Θ, pc) {
    var v1 = interpretNode(node.e1, Θ, pc);
    var v2 = interpretNode(node.e2, Θ, pc);
    return interpretApplication(v1, v2, Θ, pc);
  },

  reference: function(node, Θ, pc) {
    var v2 = interpretNode(node.expr, Θ, pc);
    var a = new Address(Σ.length);
    var v = createFacet(pc, v2, bottom);
    Σ[a.value] = v;
    return a;
  },

  dereference: function(node, Θ, pc) {
    function deref(v, pc) {
      if (v === bottom)
        return bottom;

      if (v instanceof Address)
        return Σ[v.value];

      if (v instanceof Facet) {
        if (pc.has(v.k))
          return deref(v.vh);
        if (pc.has(v.k.negation))
          return deref(v.vl);
        else
          return createFacet(v.k, deref(v.vh), deref(v.vl));
      }

      throw new InterpretError('Not an address or facet, ' + JSON.stringify(v, null, 2));
    }

    var v = interpretNode(node.expr, Θ, pc);
    var v2 = deref(v, pc);

    return v2;
  },

  assignment: function(node, Θ, pc) {
    function assign(v1, v2, pc) {
      if (v1 === bottom)
        return;

      if (v1 instanceof Address) {
        Σ[v1.value] = createFacet(pc, v2, Σ[v1.value]);
        return;
      }

      if (v1 instanceof Facet) {
        assign(v1.vh, v2, pc.add(v1.k));
        assign(v1.vl, v2, pc.add(v1.k.negation));
        return;
      }

      throw new InterpretError('Not an address or facet, ' + v1);
    }

    var v1 = interpretNode(node.left, Θ, pc);
    var v2 = interpretNode(node.right, Θ, pc);

    assign(v1, v2, pc);
    return v2;
  },

  '⟂': function(node, Θ, pc) {
	  return '⟂';
  },

  facetExpr: function(node, Θ, pc) {
    var k = new Principal(node.principal.value);

    if (pc.has(k)) {
      // F-LEFT
      return interpretNode(node.high, Θ, pc);
    }

    if (pc.has(k.negation)) {
      // F-RIGHT
      return interpretNode(node.low, Θ, pc);
    }

    // F-SPLIT
    var pc1 = pc.add(k);
    var v1 = interpretNode(node.high, Θ, pc1);

    var pc2 = pc.add(k.negation);
    var v2 = interpretNode(node.low, Θ, pc2);

    return createFacet(k, v1, v2);
  },

  // read: function(node, Θ) {

  // },

  // write: function(node, Θ) {

  // },
};

function interpretApplication(v1, v2, Θ, pc) {
  if (v1 === bottom)            // FA-BOT
    return bottom;

  if (v1 instanceof Closure) {          // FA-FUN
    var Θ2 = Object.create(v1.Θ);
    Θ2[v1.λ.variable.string] = v2;
    return interpretNode(v1.λ.body, Θ2, pc);
  }

  if (v1 instanceof Facet) {
    if (pc.has(v1.k)) {         // FA-LEFT
      return interpretApplication(v1.vh, v2, Θ, pc);
    }

    if (pc.has(v1.k.negation)) { // FA-RIGHT
      return interpretApplication(v1.vl, v2, Θ, pc);
    }

    else {                      // FA-SPLIT
      var pc1 = pc.add(v1.k);
      var vh2 = interpretApplication(v1.vh, v2, Θ, pc1);

      var pc2 = pc.add(v1.k.negation);
      var vl2 = interpretApplication(v1.vl, v2, Θ, pc2);

      return createFacet(v1.k, vh2, vl2);
    }
  }

  throw new InterpretError('Not a closure or facet, ' + JSON.stringify(v1, null, 2));
}

function interpretNode(node, Θ, pc) {
	var fn = rules[node.type];

	if (fn) return fn(node, Θ, pc);
	else throw new Error('Cannot interpret node: ' + JSON.stringify(node, null, 2));
}

function interpretProgram(AST, substitution, programCounter) {
  substitution = substitution || {};
  programCounter = programCounter || [];
  var pc = new Set(programCounter.map(function(label) { return new Principal(label); }));
  return interpretNode(AST, substitution, pc);
}

var unparse = require('./unparser');

function prettyPrint(value) {
  if (value instanceof Address)
    return ':' + value.value;

  if (value instanceof Closure) {
    var lambda = unparse(value.λ);
    if (lambda === '(λx.(λy.x))')
      lambda = true;
    if (lambda === '(λx.(λy.y))')
      lambda = false;

    return lambda;
  }

  if (value instanceof Facet)
    return '<' + value.k + ' ? ' + prettyPrint(value.vh) + ' : ' + prettyPrint(value.vl) + '>';

  return value;
}

module.exports = {
  interpret: interpretProgram,
  facet: createFacet,
  prettyPrint: prettyPrint,
};

function InterpretError(message) {
  this.message = message;
  this.name = "InterpretError";
}
