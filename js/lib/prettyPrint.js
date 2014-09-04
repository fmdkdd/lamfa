var Address = require('./address');
var Closure = require('./closure');
var Facet = require('./facet').Facet;
var TaintedValue = require('./taintedValue');
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

  if (value instanceof TaintedValue)
    return '«' + value.value + '»';

  return value;
}

module.exports = prettyPrint;
