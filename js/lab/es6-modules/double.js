let double = exports.double = (base_num) => {
  let num = {__proto__: base_num,
    eval() { return base_num.eval.call(this) * 2 }}
  return {num}}
