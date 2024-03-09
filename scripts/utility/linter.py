from os import system
from shutil import which
from subprocess import check_output

# Check if hlint or refactor is installed
if not (which('hlint') or which('refactor')):
  print('Please install hlint or refactor')
  exit(1)

# Linter utility function that takes a file path as input
# and runs hlint on it. If hlint is not installed, refactor
# is used instead.
def hlint(file: str) -> None:
  if which('hlint'):
    output = check_output(
      f"hlint {file} --refactor", 
      shell = True, 
      encoding='utf-8')
    
    if output == 'No hints': return

    write_file = open(file, 'w')
    write_file.write(output)
  elif which('refactor'):
    system(f"refactor {file}")
  else: 
    print('Please install hlint or refactor')
    exit(1)

def run_linter(file: str, step: tuple[int, int]) -> None:
  (i, n) = step
  print(f'[{i}/{n}] Running linter on {file}')
  hlint(file)
