| Command | Mean [ms] | Min [ms] | Max [ms] | Relative |
|:---|---:|---:|---:|---:|
| `python3 benchmark/index.py` | 18.5 ± 1.2 | 16.6 | 26.5 | 22.80 ± 6.42 |
| `node benchmark/index.js` | 37.0 ± 1.1 | 35.6 | 44.1 | 45.52 ± 12.55 |
| `./index-rs` | 1.1 ± 0.2 | 0.9 | 2.9 | 1.32 ± 0.45 |
| `./index-c` | 0.8 ± 0.2 | 0.7 | 6.6 | 1.00 |
| `bin/plume-vm example/closure.plm.bc` | 0.9 ± 0.2 | 0.7 | 7.5 | 1.09 ± 0.42 |
