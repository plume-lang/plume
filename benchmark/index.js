const add = (a, b) => a + b;
const sub = (a, b) => a - b;
const mul = (a, b) => a * b;
const eq = (a, b) => a == b;

function facto(n) {
  if (eq(n, 0)) {
    return 1;
  } else {
    return mul(n, facto(sub(n, 1)));
  }
}

console.log(facto(5))

function ack(m, n) {
  if (eq(m, 0)) {
    return add(n, 1);
  } else if (eq(n, 0)) {
    return ack(sub(m, 1), 1);
  } else {
    return ack(sub(m, 1), ack(m, sub(n, 1)));
  }
}

console.log(ack(3, 6))

const a = (x) => (y) => add(x, y)

console.log(a(3)(4))

const res = ack(3, 6)
console.log(res)