let num = (n) => ({type: 'num', n})
let plus = (a, b) => ({type: 'plus', a, b})
function interp(n) {
  switch (n.type) {
  case 'num':
    return n.n
    return n.n * 2
    break

  case 'plus':
    return interp(n.a) + interp(n.b)
    break
  }
}

let e1 = plus(num(1), num(2))

console.log(interp(e1))
