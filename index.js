var lexer = require('./lib/lexer');
var parser = require('./lib/parser');
var unparse = require('./lib/unparser');
var desugar = require('./lib/desugar');
var stdInterpret = require('./lib/interpreter-standard');
var fctInterpret = require('./lib/interpreter-facets');

module.exports = {
	lexer: lexer,
	parser: parser,
	unparse: unparse,
	desugar: desugar,
	interpretStandard: stdInterpret.interpret,
  interpret: fctInterpret.interpret,
  interpreter: fctInterpret,

	parse: function(source) {
		return parser.parse(lexer.lex(source));
	},
}
