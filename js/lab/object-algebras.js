/* eslint-disable */

/// Object algebras
// My attempt at understanding this talk:
// https://www.youtube.com/watch?v=snbsYyBS4Bs

function someItemPath(algebra) {
  with (algebra) {
    return chained(segment("item"), segment("123abc"))
  }
}

var pathClient = {
  segment(value) { return encodeURIComponent(value) },
  chained(first, second) { return `${first}/${second}` },
}

someItemPath(pathClient) //: "item/123abc"

var pathRouting = {
  segment(value) { return incoming => decodeURIComponent(incoming) == value },
  chained(first, second) {
    return incoming => {
      var [hd,...tl] = incoming.split('/')
      tl = tl.join('/')
      return tl && first(hd) && second(tl)
    }
  }
}

var isSomeItem = someItemPath(pathRouting) //: undefined
isSomeItem('foo/bar') //: false
isSomeItem('item/123abc') //: true

// So, actually it looks /a lot/ like what I did for FOAL.  The main difference
// being that I composed the algebras using `with`.  And instead of creating a
// result through a function like ~someItemPath~, I used the value immediately.

// Let's look at how it would work for the interpreter example.

function e1(interp) {
  with (interp) {
    return num(3)
  }
}

function e2(interp) {
  with (interp) {
    return plus(num(1), num(2))
  }
}

// Identity algebra, builds an AST

var ast = {
  num(n) { return {type: 'num', n} },
  plus(a, b) { return {type: 'plus', a, b}},
}

e1(ast) //: Object {type:"num",n:3}
e2(ast) //: Object {type:"plus",a:Object {type:"num",n:1},b:Object {type:"num",n:2}}

// More readable

var rast = {
  num(n) { return `(num ${n})` },
  plus(a, b) { return `(plus ${a} ${b})` },
}

e1(rast) //: "(num 3)"
e2(rast) //: "(plus (num 1) (num 2))"

// Standard eval

var evall = {
  num(n) { return n },
  plus(a, b) { return a + b },
}

e1(evall) //: 3
e2(evall) //: 3

// Delayed eval with thunks

var deval = {
  num(n) { return _ => n },
  plus(a, b) { return _ => a() + b() },
}

e1(deval) //: function
e1(deval)() //: 3
e2(deval)() //: 3

// Delayed eval with objects

var devalo = {
  num(n) { return {eval() { return n }}},
  plus(a, b) { return {eval() { return a.eval() + b.eval() }}},
}

e1(devalo) //: Object {eval:function eval}
e1(devalo).eval() //: 3
e2(devalo).eval() //: 3

// Pretty printer

var pp = {
  num(n) { return `${n}` },
  plus(a, b) { return `${a} + ${b}` },
}

e1(pp) //: "3"
e2(pp) //: "1 + 2"

// Double

function double(base) {
  return {__proto__: base,
    num(n) { with(base) { return num(n) * 2 }},
  }
}

e1(double(evall)) //: 6
e2(double(evall)) //: 6

// Multi double
e2(double(double(double(evall)))) //: 24

// Double + pp
e1(double(pp)) //: 6
e2(double(pp)) //: "2 + 4"

// Double + delayed eval
e2(double(deval)) //: function
e2(double(deval))() //: TypeError: a is not a function

// Does not work because double does not expose thunks
// But what if deval was an extension?

function fdeval(base) {
  return {__proto__: base,
    num(n) { return _ => { with(base) { return num(n) }}},
    plus(a, b) { return _ => { with(base) { return plus(a(), b()) }}},
  }
}

e2(fdeval(double(evall)))() //: 6

// State

function state(base) {
  var count = 0

  return {__proto__: base,
    num(n) { count++; with (base) { return num(n) }},
    plus(a, b) { count++; with (base) { return plus(a, b) }},
    getCount() { return count },
  }
}

e1(state(evall)) //: 3
e2(state(evall)) //: 3

// Need a new program to see count

function e3(interp) {
  with (interp) {
    return [
      getCount(),
      e2(interp),
      getCount()
    ]
  }
}

e3(state(evall)) //: [0,3,3]

// state + double
e3(state(double(evall))) //: [0,6,3]
e3(double(state(evall))) //: [0,6,3]

// state + pp + double
e3(double(state(pp))) //: [0,"2 + 4",3]
e3(state(double(pp))) //: [0,"2 + 4",3]



// Add if

function ifte() {
  var stdout = []

  return {
    yes: true,
    no: false,
    iff(c, t, e) { if (c) { return t } else { return e }},
    print(s) { stdout.push(s) },
    flush() { return stdout.join('\n') }
  }
}

function e_with_if(interp) {
  with (interp) {
    iff(yes, print(2), print(3))
    return flush()
  }
}

e_with_if(ifte()) //: "2 3"
// not good, both branches are evaluated anyw

// Add delayed if

function difte() {
  var stdout = []

  return {
    yes() { return true },
    no() { return false },
    iff(c, t, e) { return _ => { if (c()) { return t() } else { return e() }}},
    print(s) { return _ => stdout.push(s) },
    flush() { return _ => stdout.join('\n') },
    seq(l, r) { return _ => { l(); return r() }},
  }
}

function e_with_if(interp) {
  with (interp) {
    return seq(
      iff(yes, print(2), print(3)),
      flush())
  }
}

e_with_if(difte())() //: "2"
