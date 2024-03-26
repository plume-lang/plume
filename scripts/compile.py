from os import system
from shutil import which
import os.path
from glob import glob
import platform
import sys

ext = '.exe' if platform.system() == 'Windows' else ''

# Check for Plume compiler and VM

if (not os.path.isdir('bin') or 
    not os.path.isfile(f'bin/plume-language{ext}') or 
    not os.path.isfile(f'bin/plume-vm{ext}')):
  print('Please compile the project first')
  exit(1)

# Run the compiler and VM

file = sys.argv[1]

system(f'bin/plume-language{ext} {file}')
system(f'bin/plume-vm{ext} {file}.bc')
