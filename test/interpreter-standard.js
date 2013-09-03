var should = require('should');

var lamfa = require('../');
var interpret = lamfa.interpretStandard;
var desugar = lamfa.desugar;
var parse = lamfa.parse;
var nodes = lamfa.parser.nodes;

function testInterpret(source, expect, environment) {
	test(source, function() {
    var value = interpret(parse(source), environment);
    should.exist(value);
		value.should.equal(expect);
  });
}

suite('interpreter', function() {
	testInterpret('2', 2);

	testInterpret('x', 3, {x: 3});

	testInterpret('[(λx.x) 2]', 2);

	testInterpret('[(λx.x) y]', 'y', {y: 'y'});

	testInterpret('[(λx.x) x]', 'x', {x: 'x'});

	testInterpret('[(λx.x := 2) 0]', 2);

	testInterpret('[[(λx.(λy.x)) 1] 2]', 1);

	testInterpret('[[(λx.(λy.y)) 1] 2]', 2);

  testInterpret('[(λa.!a) ref 42]', 42);
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
    }).should.throw(/Not a closure, 2/);
  });

  test('not in scope', function() {
    (function() {
      interpret(parse('x'));
    }).should.throw(/Not in scope, x/);
  });

  test('not an address', function() {
    (function() {
      interpret(parse('!2'));
    }).should.throw(/Not an address, 2/);
  });

});
