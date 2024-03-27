rustc benchmark/index.rs -C opt-level=3 -o index-rs
# gcc -O3 benchmark/index.c -o index-c
bin/plume-language
hyperfine \
  "python3 benchmark/index.py" \
  "node benchmark/index.js" \
  "./index-rs" \
  "bin/plume-vm example/closure.bin" \
  --warmup 3 --export-markdown docs/BENCHMARK_RESULTS.md -N