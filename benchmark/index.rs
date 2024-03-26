fn facto(n: u64) -> u64 {
    if n == 0 {
        1
    } else {
        n * facto(n - 1)
    }
}

fn main() {
    println!("{}", facto(20));
}
