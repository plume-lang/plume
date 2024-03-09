from os import system
from shutil import which

# Check if fourmolu is installed
if not which('fourmolu'):
  print('Please install fourmolu')
  exit(1)

# Formatter utility function that takes a file path as input
# and runs fourmolu on it.
def fourmolu(file: str) -> None:
  if which('fourmolu'):
    system(f"fourmolu -i {file}")
  else:
    print('Please install fourmolu')
    exit(1)

def run_formatter(file: str, step: tuple[int, int]) -> None:
  (i, n) = step
  print(f'[{i}/{n}] Running formatter on {file}')
  fourmolu(file)



