let plus = exports.plus = {
  new(l, r) { return {__proto__: this, l, r} },
  eval() { return this.l.eval() + this.r.eval() }}
