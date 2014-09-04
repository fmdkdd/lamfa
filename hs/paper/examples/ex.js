var terms = {
  Constant: function(i) { return { type: 'Constant', i: i }; },
  Plus: function(a, b) { return { type: 'Plus', a: a, b: b }; },
};

var values = {
  Number: function(i) { return { type: 'Number', value: i }},
};

var interp = match({
  Constant: function(term) {
    return values.Number(term.i + 1);
  },

  Plus: function(term) {
    return plus(interp(term.a), interp(term.b));
  },
});

var plus = match({
  'Number-Number': function(a, b) {
    return values.Number(a.value + b.value);
  },
});

function match(cases) {
  var f = function() {
    var args = [].slice.call(arguments);
    args.unshift(f.cases);
    return dispatch.apply(this, args);
  };

  f.cases = cases;

  return f;
}

function dispatch(cases) {
  var rest = [].slice.call(arguments, 1);
  var signature = rest.map(function(o) { return o.type; }).join('-');
  return cases[signature].apply(this, rest);
}

// Test

var test1 = terms.Plus(terms.Constant(2), terms.Plus(terms.Constant(2), terms.Constant(3)));

console.log(interp(test1), values.Number(10));

var Interpreter = {
  interp: interp,
};

console.log(Interpreter.interp(test1), values.Number(10));

// Test instrumentation

var test2 = terms.Constant(2);

console.log(interp(test2), values.Number(3));

interp.cases.Constant = around(interp.cases.Constant, function(proceed, term) {
  return values.Number(proceed(term).value * 42);
});

console.log(interp(test2), values.Number(126));

interp.cases.Constant = around(interp.cases.Constant, function(proceed, term) {
  return proceed(terms.Constant(term.i * 42));
});

console.log(interp(test2), values.Number(3570));

function around(f, advice) {
  return function() {
    var proceed = function() { return f.apply(null, arguments); };
    var args = [].slice.call(arguments);
    args.unshift(proceed);
    return advice.apply(null, args);
  };
}

interp.cases = {
  Constant: function(term) {
    return values.Number(term.i);
  },

  Plus: function(term) {
    return plus(interp(term.a), interp(term.b));
  },
};

console.log(interp(test1), values.Number(7));
