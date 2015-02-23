// ES6 module syntax does not support parameterized modules, as far as
// I know.

// SpiderMonkey C36.0a1 DIY modules
let exports = {}
load("base.js")
load("plus.js")
load("show.js")
let { show } = exports
let { num, plus } = show({num: exports.num, plus: exports.plus})

print(plus.new(num.new(1), num.new(2)).show())
