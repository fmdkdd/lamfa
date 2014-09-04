var should = require('should');

var Address = require('../lib/address')
var Closure = require('../lib/closure');
var interpret = require('../lib/interpreter-tainting').interpret;
var nodes = require('../lib/ASTnodes');

function testInterpret(name, AST, expectedValue, expectedStore, options) {
  test(name, function() {
    var result = interpret(AST, options);
    should.exist(result, 'Undefined result object');
    should.exist(result.value, 'Undefined result value');
    should.exist(result.store, 'Undefined store');
    result.value.should.eql(expectedValue);
    result.store.elements.should.eql(expectedStore);
  });
}

suite('interpret AST node', function() {
	testInterpret('constant',
                nodes.constant(2), 2, []);

	testInterpret('variable',
                nodes.variable('x'), 3, [], {env: {x: 3}});

  test('variable not in scope', function() {
    (function() {
      interpret(nodes.variable('x'));
    }).should.throw(/Not in scope, x/);
  });

  var λ = nodes.abstraction(nodes.variable('x'),
                            nodes.variable('x'));
	testInterpret('abstraction',
                λ,
                new Closure(λ, {}), []);


	testInterpret('abstraction with substitution',
                λ,
                new Closure(λ, {x: 3}), [], {env: {x: 3}});

	testInterpret('application',
                nodes.application(λ, nodes.constant(3)),
                  3, []);

  var λbang = nodes.abstraction(nodes.variable('x'),
                                nodes.dereference(nodes.variable('x')));
	testInterpret('application with side effect',
                nodes.application(λbang, nodes.reference(nodes.constant(42))),
                42, [42]);

  test('application of constant', function() {
    (function() {
      interpret(nodes.application(
                  nodes.constant(1),
                  nodes.constant(3)));
    }).should.throw(/Not a closure or tainted value, 1/);
  });

  test('reference a constant', function() {
    var result = interpret(nodes.reference(nodes.constant(1)));
    should.exist(result);
    should.exist(result.value);
    result.value.should.be.an.instanceOf(Address);
    result.store.elements.should.include(1);
  });

  test('reference an address', function() {
    var result = interpret(nodes.reference(nodes.reference(nodes.constant(1))));
    should.exist(result);
    should.exist(result.value);
    result.value.should.be.an.instanceOf(Address);
    result.store.elements.should.include(1);
    result.store.elements.should.includeEql(new Address(0));
  });

  test('reference a closure', function() {
    var λ = nodes.abstraction(nodes.constant(1), nodes.constant(1));
    var result = interpret(nodes.reference(λ));
    should.exist(result);
    should.exist(result.value);
    result.value.should.be.an.instanceOf(Address);
    result.store.elements.should.includeEql(new Closure(λ, {}));
  });

  testInterpret('dereference address',
                nodes.dereference(nodes.reference(nodes.constant(1))),
                1, [1]);

  test('dereference not an address', function() {
    (function() {
      interpret(nodes.dereference(nodes.constant(1)));
    }).should.throw(/Not an address, 1/);
  });

  testInterpret('assign 3 to a reference to 0',
               nodes.assignment(nodes.reference(nodes.constant(0)),
                                nodes.constant(3)),
                3, [3]);

  test('assign 3 to not a reference', function() {
    (function() {
      interpret(nodes.assignment(nodes.constant(1), nodes.constant(3)));
    }).should.throw(/Not an address, 1/);
  });
});

var TaintedValue = require('../lib/taintedValue');

suite('interpret (tainted values) with PC set', function() {
  test('ref 42', function() {
    var result = interpret(nodes.reference(nodes.constant(42)),
                          {pc: [1]});
    should.exist(result);
    should.exist(result.value);
    result.value.should.be.an.instanceOf(Address);
    result.store.elements.should.includeEql(42);
  });
});

suite('interpret (tainted values) AST node', function() {
  testInterpret('taintExpr',
                nodes.taintExpr(nodes.constant(42)),
                new TaintedValue(42), []);

  testInterpret('taintExpr with PC set',
                nodes.taintExpr(nodes.constant(42)),
                new TaintedValue(42), [], {pc: [1]});

  testInterpret('taintExpr with negative PC',
                nodes.taintExpr(nodes.constant(42)),
                new TaintedValue(42), [], {pc: [-1]});

  testInterpret('dereference a tainted value',
                nodes.dereference(
                  nodes.taintExpr(nodes.reference(nodes.constant(42)))),
                new TaintedValue(42),
                [42]);

  testInterpret('assign to a tainted value',
               nodes.assignment(
                 nodes.taintExpr(nodes.reference(nodes.constant(6))),
                 nodes.constant(42)),
                42, [42]);

  var λ1 = nodes.abstraction(nodes.variable('x'),
                             nodes.variable('x'));
  var λ2 = nodes.abstraction(nodes.variable('x'),
                             nodes.constant(2));
  testInterpret('apply a tainted function',
                nodes.application(
                  nodes.taintExpr(λ1),
                  nodes.constant(0)),
                new TaintedValue(0), []);


  var desugar = require('../lib/desugar');
  testInterpret('if <true> then 2 else 3',
                desugar(nodes.ifThenElse(nodes.taintExpr(nodes.true()),
                                         nodes.constant(2),
                                         nodes.constant(3))),
                new TaintedValue(2), []);
});

var prettyPrint = require('../lib/prettyPrint');

function testPP(name, AST, expectedOutput) {
  test(name, function() {
    var output = prettyPrint(interpret(AST).value);
    should.exist(output);
    output.should.equal(expectedOutput);
  });
}

suite('interpret pretty print', function() {
  testPP('(λx.x)',
    nodes.abstraction(nodes.variable('x'), nodes.variable('x')),
    '(λx.x)');

  testPP('«2»',
    nodes.taintExpr(nodes.constant(2)),
         '«2»');

  testPP('ref 12',
         nodes.reference(nodes.constant(12)),
         '#0');

});
