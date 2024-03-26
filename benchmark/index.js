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

console.log(facto(20))