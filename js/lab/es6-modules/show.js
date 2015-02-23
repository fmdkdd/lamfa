let show = exports.show = (base) => {
  let num = {__proto__: base.num
             ,show() { return this.n.toString() }}

  let plus = {__proto__: base.plus
              ,show() { return this.l.show() + '+' + this.r.show() }}

  return {num, plus} }
