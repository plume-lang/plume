from os import system
from shutil import which
import os.path
from glob import glob
import platform
from sys import argv
import pathlib

is_root = argv[1] == '--root' if len(argv) > 1 else False

# Check for Cabal and XMake 
if not which('cabal') or not which('node'):
  print('Please install cabal, node.js')
  exit(1)

# Build the compiler project
system('cabal build')

ext = '.exe' if platform.system() == 'Windows' else ''

executable_name = f"plume{ext}"

found_executables = glob(f"dist-newstyle/**/{executable_name}", recursive=True)
executable_files = [file for file in found_executables if os.path.isfile(file)]

if len(executable_files) == 0:
  print('No executable found')
  exit(1)

executable = executable_files[0]
executable_out = f"plumec{ext}"

if not os.path.isdir('bin'): os.mkdir('bin')

system(f"cp {executable} bin/{executable_out}")

# Testing the compiler

dir = pathlib.Path(__file__).parent.parent.resolve() / "standard"
os.environ['PLUME_PATH'] = str(dir)

system('bin/plumec -e js example/iterative/fibonacci.plm')
system('node example/iterative/fibonacci.js')

print("Build succeeded!")