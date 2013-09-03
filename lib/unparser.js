var unparseFunctions = {
	application: function(node) {
		return '[' + unparseNode(node.e1) + ' ' + unparseNode(node.e2) + ']';
	},

	abstraction: function(node) {
		return '(λ' + unparseNode(node.variable) + '.' + unparseNode(node.body) + ')';
	},

	constant: function(node) {
		return node.value;
	},

	variable: function(node) {
		return node.string;
	},

	'if-then': function(node) {
		return 'if (' + unparseNode(node.condition) + ') then ' + unparseNode(node.thenBranch);
	},

	'let-in': function(node) {
		return 'let ' + unparseNode(node.variable) + ' = ' + unparseNode(node.varBody) + ' in ' + unparseNode(node.body);
	},

  reference: function(node) {
	  return 'ref ' + unparseNode(node.expr);
  },

  dereference: function(node) {
	  return '!' + unparseNode(node.expr);
  },

	sequence: function(node) {
		return unparseNode(node.left) + '; ' + unparseNode(node.right);
	},

	assignment: function(node) {
		return unparseNode(node.left) + ' := ' + unparseNode(node.right);
	},

	false: function(node) {
		return 'false';
	},

	true: function(node) {
		return 'true';
	},

  '⟂': function(node) {
	  return '⟂';
  },

  facetExpr: function(node) {
	  return '<' + unparseNode(node.principal) + ' ? ' +
      unparseNode(node.high) + ' : ' + unparseNode(node.low) + '>';
  },
};

function unparseNode(node) {
	var fn = unparseFunctions[node.type];

	if (fn) return fn(node);
	else throw new Error('Cannot unparse node: ' + JSON.stringify(node, null, 2));
}

module.exports = unparseNode;
