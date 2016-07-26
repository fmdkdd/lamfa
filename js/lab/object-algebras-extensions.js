/* eslint-disable */

var num = {
  new(n) { return {__proto__: this, n }},
  eval() { return this.n },
}

var plus = {
  new(a, b) { return {__proto__: this, a, b }},
  eval() { return this.a.eval() + this.b.eval() },
}

var base = {num, plus}

var e2 = function(modules) {
  with (modules) {
    return plus.new(num.new(1), num.new(2))
  }
}

e2({}).eval() //: 3

var double = function (base) {
  var num = {__proto__: base.num,
    eval() { return base.num.eval.call(this) * 2 },
  }

  return {num}
}

// Can compose
e2(double(base)).eval() //: 6
e2(double(double(base))).eval() //: 12
e2(double(double(double(base)))).eval() //: 24

// Composed modules are first class
var ddd = double(double(double(base)))

e2(ddd).eval() //: 24

// Programs are first class
// Modules are first class

// What about composing languages, rather than extensions?

function count() {
  var c = 0

  var set = {
    new(n) { return {__proto__: this, n }},
    eval() { c = this.n },
  }

  var get = {
    new() { return {__proto__: this }},
    eval() { return c },
  }

  var inc = {
    new() { return {__proto__: this }},
    eval() { c += 1 },
  }

  return {set, inc, get}
}

var e3 = function(modules) {
  with (modules) {
    set.new(-1)
    inc.new()
    inc.new()
    return plus.new(num.new(10), get.new())
  }
}

e3(count()).b.eval() //: 0

// At this point, we need a `seq` construct, we cannot merely
// use the control flow of the host language for the DSL.  Here
// set, inc, and inc are never evaluated.

// But let's say we have an immediate counter language

function icount() {
  var c = 0

  return {
    set(n) { c = n },
    inc() { c += 1 },
    get() { return c },
  }
}

var e4 = function(modules) {
  with (modules) {
    set(-1)
    inc()
    inc()
    return plus.new(num.new(10), num.new(get()))
  }
}

e4(icount()).eval() //: 11
