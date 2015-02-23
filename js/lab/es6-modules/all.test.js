// ES6 module syntax does not support parameterized modules, as far as
// I know.

// SpiderMonkey C36.0a1 DIY modules
let exports = {}
load("base.js")
load("plus.js")
load("show.js")
load("double.js")
load("state.js")
let { show, double, state } = exports
let { num, plus, getPC } = state(show({num: double(exports.num).num, plus: exports.plus}))

print(getPC())
let n = plus.new(num.new(1), num.new(2))
print(n.eval())
print(getPC())
print(n.show())
