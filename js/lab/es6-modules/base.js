let num = exports.num = {
  new(n) { return {__proto__: this, n} }
  ,eval() { return this.n }}
