const fetch = require('node-fetch');
const fs = require('fs');
const readlineSync = require('readline-sync');
const childProcess = require('child_process');
const deasync = require('deasync');

module.exports = {
  to_string: (e) => e.toString(),
  add_str: (a, b) => a + b,
  mul_str: (a, b) => a * b,
  string_length: (s) => s.length,
  eq_string: (a, b) => a === b,
  get_index_str: (s, i) => s[i],
  str_slice: (s, start, end) => s.slice(start, end),
  show_bool: (b) => b ? "true" : "false",
  read_file: (path) => fs.readFileSync(path, 'utf-8'),
  write_file: (path, content) => fs.writeFileSync(path, content),
  append_file(path, content) {
    return fs.appendFileSync(path, content)
  },
  does_file_exist(path) {
    return fs.existsSync(path)
  },
  print: (s) => process.stdout.write(s),
  println: (s) => console.log(s),
  get_args: () => process.argv.slice(2),
  execute_command: (cmd) => childProcess.execSync(cmd).toString(),
  input: () => readlineSync.question(),
  add_int: (a, b) => a + b,
  sub_int: (a, b) => a - b,
  mul_int: (a, b) => a * b,
  div_int: (a, b) => a / b,
  mod_int: (a, b) => a % b,

  float_to_int: (f) => parseInt(f),
  int_to_float: (i) => parseFloat(i),

  eq_int: (a, b) => a === b,
  lt_int: (a, b) => a < b,

  add_float: (a, b) => a + b,
  sub_float: (a, b) => a - b,
  mul_float: (a, b) => a * b,
  div_float: (a, b) => a / b,
  mod_float: (a, b) => a % b,
  pow_float: (a, b) => Math.pow(a, b),
  eq_float: (a, b) => a === b,
  lt_float: (a, b) => a < b,

  list_append: (l, e) => l.concat(e),
  list_prepend: (l, e) => [e].concat(l),
  list_concat: (a, b) => a.concat(b),
  ffi_get_index: (l, i) => l[i],
  ffi_slice_list: (l, start, end) => l.slice(start, end),

  char_to_string: (c) => c,
  eq_char: (a, b) => a === b,

  wait: blockForPromiseSync,
  'async': async (f) => await f,

  fetch: async(url) => {
    try {
      const res = await fetch(url);
      const txt = await res.text();

      return [null, "Result", "Ok", txt];
    } catch (e) {
      return [null, "Result", "Error", e.toString()];
    }
  }
}

function blockForPromiseSync(p) {
    let result= undefined;
    let error= undefined;

    p.then(value => { result = value })
        .catch(err => { error = err })

    deasync.loopWhile(() =>
        result === undefined && error === undefined)

    if (error !== undefined) {
        throw error
    }
    return result
}