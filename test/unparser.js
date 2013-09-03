var lamfa = require('../');
var parse = lamfa.parse;
var unparse = lamfa.unparse;

suite('parse/unparse', function() {
	function sanityCheck(source) {
		test('unparse . parse . ' + source + ' == ' + source, function() {
			unparse(parse(source)).should.eql(source);
		});

		test('parse . unparse . parse . ' + source + ' == ' + source, function() {
			parse(unparse(parse(source))).should.eql(parse(source));
		});
	}

	sanityCheck('!x');
	sanityCheck('ref x');
	sanityCheck('[x z := e]');
	sanityCheck('z := true; z');
	sanityCheck('[[a a] a]');
	sanityCheck('if (x) then x; if (if (x) then x) then x');
	sanityCheck('[(λx.[y z]) e]');
	sanityCheck('let a = false in let b = true in z');
});

suite('Errors', function() {
	test('Stupid node', function() {
		(function() {
			unparse({})
		}).should.throw('Cannot unparse node: {}');
	});
});
