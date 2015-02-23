// ES6 module syntax
// import { num } from "base"

// SpiderMonkey C36.0a1 DIY modules
let exports = {}
load("base.js")
let { num } = exports

let e1 = num.new(3)
print(e1.eval())
