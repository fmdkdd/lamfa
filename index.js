var lexer = require('./lib/lexer');
var parser = require('./lib/parser');
var unparse = require('./lib/unparser');
var desugar = require('./lib/desugar');
var stdInterpret = require('./lib/interpreter-standard');
var fctInterpret = require('./lib/interpreter-facets');
var prettyPrint = require('./lib/prettyPrint.js');

module.exports = {
	lexer: lexer,
	parser: parser,
	unparse: unparse,
	desugar: desugar,
	interpretStandard: stdInterpret,
  interpret: fctInterpret,
  prettyPrint: prettyPrint,

	parse: function(source) {
		return parser.parse(lexer.lex(source));
	},
}
