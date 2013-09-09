var should = require('should');

var interpreter = require('../lib/interpreter-facets');
var interpret = interpreter.interpret;
var nodes = require('../lib/parser').nodes;

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
                new interpreter.Closure(λ, {}), []);


	testInterpret('abstraction with substitution',
                λ,
                new interpreter.Closure(λ, {x: 3}), [], {env: {x: 3}});

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
    }).should.throw(/Not a closure or facet, 1/);
  });

  test('reference a constant', function() {
    var result = interpret(nodes.reference(nodes.constant(1)));
    should.exist(result);
    should.exist(result.value);
    result.value.should.be.an.instanceOf(interpreter.Address);
    result.store.elements.should.include(1);
  });

  test('reference an address', function() {
    var result = interpret(nodes.reference(nodes.reference(nodes.constant(1))));
    should.exist(result);
    should.exist(result.value);
    result.value.should.be.an.instanceOf(interpreter.Address);
    result.store.elements.should.include(1);
    result.store.elements.should.includeEql(new interpreter.Address(0));
  });

  test('reference a closure', function() {
    var λ = nodes.abstraction(nodes.constant(1), nodes.constant(1));
    var result = interpret(nodes.reference(λ));
    should.exist(result);
    should.exist(result.value);
    result.value.should.be.an.instanceOf(interpreter.Address);
    result.store.elements.should.includeEql(new interpreter.Closure(λ, {}));
  });

  test('reference ⟂', function() {
    var result = interpret(nodes.reference(nodes.bottom()));
    should.exist(result);
    should.exist(result.value);
    result.value.should.be.an.instanceOf(interpreter.Address);
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
    }).should.throw(/Not an address or facet, 1/);
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
    }).should.throw(/Not an address or facet, 1/);
  });
});

var createFacet = interpreter.createFacet;

suite('interpret (facets) with PC set', function() {
  test('ref 42', function() {
    var result = interpret(nodes.reference(nodes.constant(42)),
                          {pc: [1]});
    should.exist(result);
    should.exist(result.value);
    result.value.should.be.an.instanceOf(interpreter.Address);
    result.store.elements.should.includeEql(createFacet(1, 42, '⟂'));
  });
});

suite('interpret (facets) AST node', function() {
  testInterpret('facetExpr',
                nodes.facetExpr(nodes.constant(1),
                                nodes.constant(42),
                                nodes.bottom()),
                createFacet(1, 42, '⟂'), []);

  testInterpret('facetExpr with PC set',
                nodes.facetExpr(nodes.constant(1),
                                nodes.constant(42),
                                nodes.bottom()),
                42, [], {pc: [1]});

  testInterpret('facetExpr with negative PC',
                nodes.facetExpr(nodes.constant(1),
                                nodes.constant(42),
                                nodes.bottom()),
                '⟂', [], {pc: [-1]});

  testInterpret('dereference a facet',
                nodes.dereference(
                  nodes.facetExpr(nodes.constant(1),
                                  nodes.reference(nodes.constant(42)),
                                  nodes.bottom()
                                 )),

                createFacet(1, createFacet(1, 42, '⟂'), '⟂'),
                [createFacet(1, 42, '⟂')]);

  testInterpret('assign to a facet',
               nodes.assignment(
                 nodes.facetExpr(nodes.constant(1),
                                 nodes.bottom(),
                                 nodes.reference(nodes.constant(6))),
                 nodes.constant(42)),
                42, [createFacet(1, createFacet(1, '⟂', 6), 42)]);

  var λ1 = nodes.abstraction(nodes.variable('x'),
                             nodes.variable('x'));
  var λ2 = nodes.abstraction(nodes.variable('x'),
                             nodes.constant(2));
  testInterpret('apply a facet',
                nodes.application(
                  nodes.facetExpr(nodes.constant(1), λ1, λ2),
                  nodes.constant(0)),
                createFacet(1, 0, 2), []);
});

function testPP(name, AST, expectedOutput) {
  test(name, function() {
    var output = interpreter.prettyPrint(interpret(AST).value);
    should.exist(output);
    output.should.equal(expectedOutput);
  });
}


suite('interpret pretty print', function() {
  testPP('(λx.x)',
    nodes.abstraction(nodes.variable('x'), nodes.variable('x')),
    '(λx.x)');

  testPP('<1 ? 2 : 3>',
    nodes.facetExpr(nodes.constant(1),
                    nodes.constant(2),
                    nodes.constant(3)),
         '<1 ? 2 : 3>');

  testPP('ref 12',
         nodes.reference(nodes.constant(12)),
         '#0');

});
