rustc benchmark/index.rs -C opt-level=3
bin/plume-language
hyperfine \
  "python3 benchmark/index.py" \
  "node benchmark/index.js" \
  "./index" \
  "bin/plume-vm example/closure.plm.bc" \
  --warmup 3 --export-markdown docs/BENCHMARK_RESULTS.md -N