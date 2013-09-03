var nodes = require('./parser').nodes;

var Σ = [];

var bottom = '⟂';

function closure(λ, Θ) {
  return {
    type: 'closure',
    λ: λ,
    Θ: Θ,
  };
}

function isClosure(value) {
  return typeof value === 'object' && value.type === 'closure';
}

function isConstant(value) {
  return typeof value === 'object' && value.type === 'constant';
}

function address(n) {
  return {
    type: 'address',
    value: n,
  };
}

function isAddress(value) {
  return typeof value === 'object' && value.type === 'address';
}

function Set(elems) {
  elems = elems || [];
  this.bag = {};
  if (elems.length > 0) {
    elems.forEach(function(el) { this.bag[el] = true }.bind(this));
    this.length = Object.keys(this.bag).length;

    this.first = elems[0];
    this.rest = new Set(elems.slice(1));
  }

  else {
    this.length = 0;
  }
}

Set.prototype.has = function(elem) {
	return !!this.bag[elem];
};

Set.prototype.add = function(elem) {
	return new Set(Object.keys(this.bag).concat(elem));
};

Set.prototype.isEmpty = function() {
	return this.length === 0;
};

function isSet(value) {
  return value instanceof Set;
}

function createFacet(k, vh, vl) {
  return {
    type: 'facet',
    k: k,
    vh: vh,
    vl: vl,
  };
}

function isFacet(value) {
  return typeof value === 'object' && value.type === 'facet';
}

function facet(k, vh, vl) {
  if (typeof k === 'number')
    k = new Set([k]);

  if (!isSet(k))
    throw new InterpretError('Not a set, ' + k);

  if (k.isEmpty())
    return vh;

  var l = k.first;
  var rest = k.rest;

  if (isNegation(l))
    return createFacet(l, vl, facet(rest, vh, vl));
  else
    return createFacet(l, facet(rest, vh, vl), vl);
}

function negation(k) {
  return '!' + k;
}

function isNegation(k) {
  return k[0] === '!';
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
	  return closure(node, Θ);
  },

  application: function(node, Θ, pc) {
    var v1 = interpretNode(node.e1, Θ, pc);
    var v2 = interpretNode(node.e2, Θ, pc);
    return interpretApplication(v1, v2, Θ, pc);
  },

  reference: function(node, Θ, pc) {
    var v2 = interpretNode(node.expr, Θ, pc);
    var a = address(Σ.length);
    var v = facet(pc, v2, bottom);
    Σ[a.value] = v;
    return a;
  },

  dereference: function(node, Θ, pc) {
    function deref(v, pc) {
      if (v === bottom)
        return bottom;

      if (isAddress(v))
        return Σ[v.value];

      if (isFacet(v)) {
        if (pc.has(v.k))
          return deref(v.vh);
        if (pc.has(negation(v.k)))
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

      if (isAddress(v1)) {
        Σ[v1.value] = facet(pc, v2, Σ[v1.value]);
        return;
      }

      if (isFacet(v1)) {
        assign(v1.vh, v2, pc.add(v1.k));
        assign(v1.vl, v2, pc.add(negation(v1.k)));
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
    if (pc.has(node.principal)) {
      // F-LEFT
      return interpretNode(node.high, Θ, pc);
    }

    if (pc.has(negation(node.principal))) {
      // F-RIGHT
      return interpretNode(node.low, Θ, pc);
    }

    // F-SPLIT

    var pc1 = pc.add(node.principal);
    var v1 = interpretNode(node.high, Θ, pc1);

    var pc2 = pc.add(negation(node.principal));
    var v2 = interpretNode(node.low, Θ, pc2);

    return facet(node.principal.value, v1, v2);
  },

  // read: function(node, Θ) {

  // },

  // write: function(node, Θ) {

  // },
};

function interpretApplication(v1, v2, Θ, pc) {
  if (v1 === bottom)            // FA-BOT
    return bottom;

  if (isClosure(v1)) {          // FA-FUN
    var Θ2 = Object.create(v1.Θ);
    Θ2[v1.λ.variable.string] = v2;
    return interpretNode(v1.λ.body, Θ2, pc);
  }

  if (isFacet(v1)) {
    if (pc.has(v1.k)) {         // FA-LEFT
      return interpretApplication(v1.vh, v2, Θ, pc);
    }

    if (pc.has(negation(v1.k))) { // FA-RIGHT
      return interpretApplication(v1.vl, v2, Θ, pc);
    }

    else {                      // FA-SPLIT
      var pc1 = pc.add(v1.k);
      var vh2 = interpretApplication(v1.vh, v2, Θ, pc1);

      var pc2 = pc.add(negation(v1.k));
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
  var pc = new Set(programCounter);
  return interpretNode(AST, substitution, pc);
}

var unparse = require('./unparser');

function prettyPrint(value) {
  if (isAddress(value))
    return ':' + value.value;

  if (isClosure(value)) {
    var lambda = unparse(value.λ);
    if (lambda === '(λx.(λy.x))')
      lambda = true;
    if (lambda === '(λx.(λy.y))')
      lambda = false;

    return lambda;
  }

  if (isFacet(value))
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
