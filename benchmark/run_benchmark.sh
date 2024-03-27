cd benchmark
rustc index.rs -C opt-level=3 -o index-rs
javac Main.java
# gcc -O3 benchmark/index.c -o index-c
bin/plume-language
hyperfine \
  "python3 index.py" \
  "node index.js" \
  "./index-rs" \
  "java Main" \
  "../bin/plume-vm ../example/closure.bin" \
  --warmup 3 --export-markdown ../docs/BENCHMARK_RESULTS.md -N