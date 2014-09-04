var lexer = require('../').lexer;

function testProgram(code, tokens) {
	test(code, function() {
		lexer.lex(code).should.eql(tokens.map(lexer.tokenize));
	});
}

suite('Lexer', function() {
	testProgram('a # comment', ['a']);

	testProgram('# comment\nz', ['z']);

	testProgram('⟂', ['⟂']);

	testProgram('x', ['x']);

	testProgram('2', ['2']);

	testProgram('!x', ['!', 'x']);

	testProgram('ref x', ['ref', 'x']);

  testProgram('<zob ? 42 : (λx.x)>',
              ['<', 'zob', '?', '42', ':', '(', 'λ', 'x', '.', 'x', ')', '>']);

	testProgram('[x z := e]',
					    ['[', 'x', 'z', ':=', 'e', ']']);

	testProgram('z := true; z',
					    ['z', ':=', 'true', ';', 'z']);

	testProgram('[[a a] a]',
					    ['[', '[', 'a', 'a', ']', 'a', ']']);

	testProgram('if x then x;\nif if x then x then x',
					    ['if', 'x', 'then', 'x', ';',
					     'if', 'if', 'x', 'then', 'x', 'then', 'x']);

	testProgram('[(λx.[y z]) e]',
					    ['[', '(', 'λ', 'x', '.', '[', 'y', 'z', ']', ')', 'e', ']']);

	test('number token', function() {
		lexer.tokenize('01')
			.should.eql(lexer.token('number', '01'));
	});

	test('unknown token', function() {
		(function() {
			lexer.lex('%');
		}).should.throw('Unknown token %');
	});
})
