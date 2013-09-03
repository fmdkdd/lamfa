var lamfa = require('../');
var parser = lamfa.parser;
var parse = lamfa.parse;
var nodes = parser.nodes;
var unparse = lamfa.unparse;

suite('parse', function() {
	function testParse(source, other) {
		test(source, function() {
			if (typeof other == 'string') // code
				parse(source).should.eql(parse(other));
			else						  // AST
				parse(source).should.eql(other);
		});
	}

	testParse('2', nodes.constant(2));

	testParse('x', nodes.variable('x'));

	testParse('!x', nodes.dereference(nodes.variable('x')));

	testParse('ref x', nodes.reference(nodes.variable('x')));

	testParse('ref (<1 ? x : x>)', nodes.reference(
    nodes.facetExpr(
      nodes.constant(1),
      nodes.variable('x'),
      nodes.variable('x')
    )))

	//testParse('ref <1 ? x : x>', 'ref (<1 ? x : x>)');

	testParse('e := !x',
            nodes.assignment(
              nodes.variable('e'),
              nodes.dereference(nodes.variable('x'))));

	testParse('λx.e',
				    nodes.abstraction(
					    nodes.variable('x'),
					    nodes.variable('e')));

	testParse('e := (λx.x)',
				    nodes.assignment(
					    nodes.variable('e'),
					    nodes.abstraction(nodes.variable('x'), nodes.variable('x'))
				    ));

	//testParse('e := λx.x', 'e := (λx.x)');

	testParse('[(λx.x) [(λz.z := e) e]]',
				    nodes.application(
					    nodes.abstraction(nodes.variable('x'), nodes.variable('x')),
					    nodes.application(
						    nodes.abstraction(nodes.variable('z'),
												          nodes.assignment(nodes.variable('z'),
																		               nodes.variable('e'))),
						    nodes.variable('e')
					    )
				    ));

	//testParse('[λx.x [λz.z := e e]]', '[(λx.x) [(λz.z := e) e]]');

	// testParse('e := (f := g)',
	// 			    nodes.assignment(
	// 				    nodes.variable('e'),
	// 				    nodes.assignment(nodes.variable('f'), nodes.variable('g'))
	// 			    ));

	//testParse('e := f := g', 'e := (f := g)');

	testParse('if x then if (if y then y) then x',
				    nodes.ifThen(
					    nodes.variable('x'),
					    nodes.ifThen(
						    nodes.ifThen(nodes.variable('y'), nodes.variable('y')),
						    nodes.variable('x')
					    )
				    ));

	testParse('if x then if (if y then y) then x', 'if (x) then (if (if (y) then (y)) then x)');

  testParse('<1 ? 42 : (λx.x)>',
           nodes.facetExpr(
             nodes.constant(1),
             nodes.constant(42),
             nodes.abstraction(
               nodes.variable('x'),
               nodes.variable('x')
             )
           ));

  testParse('<1 ? <2 ? 3 : 4> : 5>',
           nodes.facetExpr(
             nodes.constant(1),
             nodes.facetExpr(
               nodes.constant(2),
               nodes.constant(3),
               nodes.constant(4)
             ),
             nodes.constant(5)
           ));
});

suite('Unexpected tokens', function() {
	test('in simpleExpression()', function() {
		(function() {
			parse('.');
		}).should.throw('Unexpected token "."');
	});

	test('in expression()', function() {
		(function() {
			parse('a ;;');
		}).should.throw('Unexpected token ";"');
	});

	test('in expect()', function() {
		(function() {
			parse('λ.');
		}).should.throw('Expected token "identifier" got "."');
	});
});
