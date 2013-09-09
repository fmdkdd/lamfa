var nodes = require('./ASTnodes');

var symbolCounter = 0;

function generateSymbol() {
  return '$' + symbolCounter++;
}

var desugarFunctions = {
  sequence: function(node) {
    return desugarNode(nodes.letIn(
      nodes.variable(generateSymbol()),
      desugarNode(node.left),
      desugarNode(node.right)
    ));
  },

  'let-in': function(node) {
    return desugarNode(
      nodes.application(nodes.abstraction(node.variable, desugarNode(node.body)),
                        desugarNode(node.varBody)));
  },

  'fi-then': function(node) {
    return desugarNode(
      nodes.ifThenElse(node.condition, nodes.constant(0), node.thenBranch)
      );
  },

  'if-then': function(node) {
    return desugarNode(
      nodes.ifThenElse(node.condition, node.thenBranch, nodes.constant(0))
      );
  },

  'if-then-else': function(node) {
    return desugarNode(
      nodes.application(
        nodes.application(
          nodes.application(desugarNode(node.condition),
                            nodes.abstraction(nodes.variable('d'),
                                              desugarNode(node.thenBranch))),
          nodes.abstraction(nodes.variable('d'), desugarNode(node.elseBranch))),
        nodes.abstraction(nodes.variable('x'), nodes.variable('x'))
      ));
  },

  assignment: function(node) {
    return nodes.assignment(desugarNode(node.left), desugarNode(node.right));
  },

  reference: function(node) {
	  return nodes.reference(desugarNode(node.expr));
  },

  dereference: function(node) {
	  return nodes.dereference(desugarNode(node.expr));
  },

  true: function(node) {
    return desugarNode(
      nodes.abstraction(nodes.variable('x'),
                        nodes.abstraction(nodes.variable('y'),
                                          nodes.variable('x'))));
  },

  false: function(node) {
    return desugarNode(
      nodes.abstraction(nodes.variable('x'),
                        nodes.abstraction(nodes.variable('y'),
                                          nodes.variable('y'))));
  },

  abstraction: function(node) {
	  return nodes.abstraction(
      node.variable,
      desugarNode(node.body)
    );
  },

  facetExpr: function(node) {
	  return nodes.facetExpr(
        node.principal,
        desugarNode(node.high),
        desugarNode(node.low)
      );
  },
};

function desugarNode(node) {
  var fn = desugarFunctions[node.type];

  if (fn) return fn(node);
  else return node;
}

module.exports = desugarNode;
