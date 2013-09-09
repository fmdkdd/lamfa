var should = require('should');

var Address = require('../lib/address')
var Closure = require('../lib/closure');
var interpret = require('../lib/interpreter-standard');
var nodes = require('../lib/ASTnodes');

function testInterpret(name, AST, expectedValue, expectedStore, environment, initialStore) {
  test(name, function() {
    var result = interpret(AST, environment, initialStore);
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
                nodes.variable('x'), 3, [], {x: 3});

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
                new Closure(λ, {x: 3}), [], {x: 3});

	testInterpret('application',
                nodes.application(λ, nodes.constant(3)),
                  3, []);

	testInterpret('application of ⟂',
                nodes.application(
                  nodes.bottom(),
                  nodes.constant(3)),
                  '⟂', []);

  test('application of constant', function() {
    (function() {
      interpret(nodes.application(
                  nodes.constant(1),
                  nodes.constant(3)));
    }).should.throw(/Not a closure, 1/);
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

  test('reference ⟂', function() {
    var result = interpret(nodes.reference(nodes.bottom()));
    should.exist(result);
    should.exist(result.value);
    result.value.should.be.an.instanceOf(Address);
    result.store.elements.should.eql(['⟂']);
  });

  testInterpret('dereference address',
                nodes.dereference(nodes.reference(nodes.constant(1))),
                1, [1]);

  testInterpret('dereference ⟂',
                nodes.dereference(nodes.bottom()),
               '⟂', []);

  test('dereference not an address', function() {
    (function() {
      interpret(nodes.dereference(nodes.constant(1)));
    }).should.throw(/Not an address, 1/);
  });

  testInterpret('assign 3 to a reference to 0',
               nodes.assignment(nodes.reference(nodes.constant(0)),
                                nodes.constant(3)),
                3, [3]);

  testInterpret('assign 3 to bottom',
               nodes.assignment(nodes.bottom(),
                                nodes.constant(3)),
                3, []);

  test('assign 3 to not a reference', function() {
    (function() {
      interpret(nodes.assignment(nodes.constant(1), nodes.constant(3)));
    }).should.throw(/Not an address, 1/);
  });
});

test('interpret gibberish', function() {
  (function() {
    interpret('gibberish');
  }).should.throw(/Cannot interpret node/);
});
