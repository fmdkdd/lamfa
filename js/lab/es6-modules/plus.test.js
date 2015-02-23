// ES6 module syntax
// import { num } from "base"
// import { plus } from "plus"

// SpiderMonkey C36.0a1 DIY modules
let exports = {}
load("base.js")
load("plus.js")
let { num, plus } = exports

let e2 = plus.new(num.new(1), num.new(2));
print(e2.eval())
