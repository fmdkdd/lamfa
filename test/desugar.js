var lamfa = require('../');
var desugar = lamfa.desugar;
var parser = lamfa.parser;
var parse = lamfa.parse;
var nodes = parser.nodes;
var unparse = lamfa.unparse;

suite('desugar', function() {
	function testDesugar(source, expect) {
		test(source, function() {
			unparse(desugar(parse(source))).should.equal(expect);
		});
	}

	testDesugar('true', '(λx.(λy.x))');
	testDesugar('false', '(λx.(λy.y))');
	testDesugar('if a then b', '[[[a (λd.b)] (λd.0)] (λx.x)]');
	//testDesugar('if a then b else c', '[[[a (λd.b)] (λd.c)] (λχ.χ)]');
	testDesugar('let x = a in b', '[(λx.b) a]');
	testDesugar('a ; b', '[(λ$0.b) a]');
	testDesugar('a := b', 'a := b');
	testDesugar('<1 ? true : false>', '<1 ? (λx.(λy.x)) : (λx.(λy.y))>');
	testDesugar('ref 0', 'ref 0');
	testDesugar('!0', '!0');
});
