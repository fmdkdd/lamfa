let num = (n) => ({type: 'num', n})
let plus = (a, b) => ({type: 'plus', a, b})
function interp(n) {
  switch (n.type) {
  case 'num':
    return interp_num(n)

  case 'plus':
    return interp_plus(n)
  }
}
function interp_num({n}) { return n }
function interp_plus({a,b}) {
  return interp(a) + interp(b)
}

let e1 = plus(num(1), num(2))

console.log(interp(e1))
