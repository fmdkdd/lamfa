let state = exports.state = function(base, pc = 0) {
  let num = {__proto__: base.num
             ,eval() { pc++; return base.num.eval.call(this) }}

  let plus = {__proto__: base.plus
              ,eval() { pc++; return base.plus.eval.call(this) }}

  let getPC = () => pc

  return {num, plus, getPC}}
