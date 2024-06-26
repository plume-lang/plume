from os import system
from shutil import which
import os.path
from glob import glob
import platform
import sys
import pathlib

ext = '.exe' if platform.system() == 'Windows' else ''

# Check for Plume compiler and VM

if (not os.path.isdir('bin') or 
    not os.path.isfile(f'bin/plumec{ext}') or 
    not os.path.isfile(f'bin/plume{ext}')):
  print('Please compile the project first')
  exit(1)

# Run the compiler and VM

file = sys.argv[1]

dir = pathlib.Path(__file__).parent.parent.resolve() / "standard"
os.environ['PLUME_PATH'] = str(dir)

system(f'bin/plumec{ext} {file}')
without_ext = file.removesuffix('.plm')
system(f'bin/plume{ext} {without_ext}.bin')
