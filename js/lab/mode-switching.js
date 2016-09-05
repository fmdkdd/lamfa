// Terms
var num = (n) => ({type: 'num', n })
var plus = (a, b) => ({type: 'plus', a, b })

// Standard interpretation
var interp = {
  rules: {
    num(num) { return num.n },
    plus(plus) { return this.eval(plus.a) + this.eval(plus.b) },
  },

  eval(node) {
    return this.rules[node.type].call(this, node)
  },
}

// Test
var e1 = num(2)
interp.eval(e1) //: 2

var e2 = plus(num(1), num(2))
interp.eval(e2) //: 3

// New interp
function derive(interp, rules) {
  Object.setPrototypeOf(rules, interp.rules)
  return {__proto__: interp, rules }
}

var interp_double = derive(interp, {
  num(num) { return num.n * 2 },
})

interp_double.eval(e1) //: 4
interp_double.eval(e2) //: 6

// With super call
var interp_double_proceed = derive(interp, {
  num(num) {
    return interp.eval(num) * 2
  }
})

interp_double_proceed.eval(e1) //: 4

// Logging interp
var interp_log = {__proto__: interp,
  eval(node) {
    console.log(`eval ${node.type}`)
    return interp.eval.call(this, node)
  }
}

interp_log.eval(e1) //: 2
interp_log.eval(e2) //: 3

// The intent is to use the interp.eval, but doing that
// we *stay* in that object when recursing in rules.plus.

// Adding .call(this) is what we want to stay in the context
// of the current interpreter

// Adding a log term to toggle logging behavior
var start_trace = () => ({type: 'start_trace'})
var end_trace = () => ({type: 'end_trace'})

// Also need a sequence operator for programs
var progn = (...stmts) => ({type: 'progn', stmts })

interp.rules.progn = function(progn) {
  var result
  progn.stmts.forEach(s => { result = this.eval(s) })
  return result
}


var e3 = progn(
  plus(num(1), num(2)),
  plus(num(3), num(4))
)

console.group('83')
interp_log.eval(e3) //: 7
console.groupEnd()

var interp_traceable = derive(interp, {
  start_trace() {
    Object.setPrototypeOf(this, interp_log)
  },
})

// Must add the transition log->traceable on stop_trace
interp_log.rules.end_trace = function() {
  Object.setPrototypeOf(this, interp_traceable)
}

var e4 = progn(
  plus(num(1), num(2)),
  start_trace(),
  plus(num(3), num(4)),
  end_trace(),
  num(42)
)

var my_interp = Object.create(interp_traceable)

console.group('107')
my_interp.eval(e4) //: 42
console.groupEnd()
