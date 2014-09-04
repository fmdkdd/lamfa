var InterpretError = require('./error.js');
var Set = require('./set');
var Principal = require('./principal');

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

module.exports = {
  Facet: Facet,
  createFacet: createFacet,
};
