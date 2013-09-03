var should = require('should');

var lamfa = require('../');
var interpret = lamfa.interpret;
var desugar = lamfa.desugar;
var parse = lamfa.parse;
var nodes = lamfa.parser.nodes;

function testInterpret(source, expect, environment, programCounter) {
	test(source, function() {
    var value = interpret(parse(source), environment, programCounter);
    should.exist(value);
		value.should.eql(expect);
  });
}

suite('interpreter', function() {
	testInterpret('2', 2);

	testInterpret('x', 3, {x: 3});

	testInterpret('[(λx.x) 2]', 2);

	testInterpret('[(λx.x) y]', 'y', {y: 'y'});

	testInterpret('[(λx.x) x]', 'x', {x: 'x'});

	testInterpret('[(λx.x := 2) ref 0]', 2);

  testInterpret('[(λy.[(λ$0.!y) [[[(λx.(λy.x)) (λd.y := 2)] (λd.0)] (λx.x)]]) ref 0]', 2);

	testInterpret('[[(λx.(λy.x)) 1] 2]', 1);

	testInterpret('[[(λx.(λy.y)) 1] 2]', 2);

  testInterpret('[(λa.!a) ref 42]', 42);
});

var facet = lamfa.interpreter.facet;

suite('facets', function() {
	testInterpret('<1 ? 42 : ⟂>', facet(1, 42, '⟂'));

	testInterpret('[(λa.!a) ref 42]', facet(1, 42, '⟂'), null, [1]);

	testInterpret('[<1 ? (λx.x) : ⟂> 4]', facet(1, 4, '⟂'));

  // let z = ref 1 in (if <1 ? false : true> then z := 0; !z)
  testInterpret('[(λz.[(λ$0.!z) [[[<1 ? (λx.(λy.y)) : (λx.(λy.x))> (λd.z := 0)] (λd.0)] (λx.x)]]) ref 1]',
                facet(1, 1, 0));
});

suite('bottom', function() {
  	testInterpret('⟂', '⟂');

  	testInterpret('[⟂ 2]', '⟂');

  	testInterpret('!⟂', '⟂');

  	testInterpret('⟂ := 2', 2);
});

suite('errors', function() {

  test('not a closure', function() {
    (function() {
      interpret(parse('[2 2]'));
    }).should.throw(/Not a closure or facet, 2/);
  });

  test('not in scope', function() {
    (function() {
      interpret(parse('x'));
    }).should.throw(/Not in scope, x/);
  });

  test('not an address', function() {
    (function() {
      interpret(parse('!2'));
    }).should.throw(/Not an address or facet, 2/);
  });

});
