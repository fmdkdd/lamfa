var nodes = {
  constant: nodeMaker('constant', 'value'),
  variable: nodeMaker('variable', 'string'),
  abstraction: nodeMaker('abstraction', 'variable', 'body'),
  application: nodeMaker('application', 'e1', 'e2'),
  reference: nodeMaker('reference', 'expr'),
  dereference: nodeMaker('dereference', 'expr'),
  assignment: nodeMaker('assignment', 'left', 'right'),
  sequence: nodeMaker('sequence', 'left', 'right'),
  fiThen: nodeMaker('fi-then', 'condition', 'thenBranch'),
  ifThen: nodeMaker('if-then', 'condition', 'thenBranch'),
  ifThenElse: nodeMaker('if-then-else', 'condition', 'thenBranch', 'elseBranch'),
  letIn: nodeMaker('let-in', 'variable', 'varBody', 'body'),
  true: nodeMaker('true'),
  false: nodeMaker('false'),
  bottom: nodeMaker('⟂'),
  facetExpr: nodeMaker('facetExpr', 'principal', 'high', 'low'),
};

function nodeMaker(type) {
  var rest = Array.prototype.slice.call(arguments, 1);
  rest.unshift('type');
  return objectCreator.apply(this, rest).bind(undefined, type);
}

function objectCreator() {
  var names = Array.prototype.slice.call(arguments);

  return function() {
    var args = Array.prototype.slice.call(arguments);

    var o = {};
    for (var i in names) {
      o[names[i]] = args[i];
    }

    return o;
  }
}

module.exports = nodes;
