// ES6 module syntax does not support parameterized modules, as far as
// I know.

// SpiderMonkey C36.0a1 DIY modules
let exports = {}
load("base.js")
load("plus.js")
load("state.js")
let { state } = exports
let { num, plus, getPC } = state({num: exports.num, plus: exports.plus})

print(getPC())
print(plus.new(num.new(1), num.new(2)).eval())
print(getPC())
