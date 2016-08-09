var num = {
  new(n) { return {__proto__: num, n }},
  visit(p) { return p.forNum(this) },
}

var plus = {
  new(a, b) { return {__proto__: plus, a, b }},
  visit(p) { return p.forPlus(this) },
}

var eval = {
  forNum(n) { return n.n },
  forPlus(p) { return p.a.visit(this) + p.b.visit(this) },
}

num.new(2).visit(eval) //: 2
plus.new(num.new(1), num.new(2)).visit(eval) //: 3

// Add mult
var mult = {
  new(a, b) { return {__proto__: mult, a, b }},
  visit(p) { return p.forMult(this) },
}

// Add eval for mult

var evalForMult = {
  __proto__: eval,
  forMult(m) { return m.a.visit(this) * m.b.visit(this) },
}

// Works
mult.new(num.new(1), num.new(2)).visit(evalForMult) //: 2

// And even recursively
mult.new(mult.new(num.new(1), num.new(2)), num.new(2)).visit(evalForMult) //: 4

// But, if the visitor has a context

var count = {
  new(c) { return {__proto__: count, c }},
  forNum() { this.c += 1 },
  forPlus(p) { this.c = p.a.visit(this.new(this.c)) + p.b.visit(this.new(this.c)) },
}

num.new(2).visit(count) //: undefined
plus.new(num.new(1), num.new(2)).visit(count) //: undefined

var countForMult = {
  __proto__: count,
  forMult(p) { this.c = p.a.visit(this.new(this.c)) * p.b.visit(this.new(this.c)) },
}

// Does not work, because this.new will return a count, not a countForMult
mult.new(mult.new(num.new(1), num.new(2)), num.new(2)).visit(countForMult)
//! TypeError: p.forMult is not a function

// Redefine the abstract constructor (extensible visitor)
var countForMult2 = {
  __proto__: count,
  new(c) { return {__proto__: countForMult2, c }},
  forMult(p) { this.c = p.a.visit(this.new(this.c)) * p.b.visit(this.new(this.c)) },
}

mult.new(mult.new(num.new(1), num.new(2)), num.new(2)).visit(countForMult2)
//: undefined

// But having `this` as proto also works
var count3 = {
  new(c) { return {__proto__: this, c }},
  forNum() { this.c += 1 },
  forPlus(p) { this.c = p.a.visit(this.new(this.c)) + p.b.visit(this.new(this.c)) },
}

num.new(2).visit(count3) //: undefined
plus.new(num.new(1), num.new(2)).visit(count3) //: undefined

var countForMult3 = {
  __proto__: count3,
  forMult(p) { this.c = p.a.visit(this.new(this.c)) * p.b.visit(this.new(this.c)) },
}

mult.new(mult.new(num.new(1), num.new(2)), num.new(2)).visit(countForMult3)
//: undefined
