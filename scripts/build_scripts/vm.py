from os import system
from shutil import which
import os.path
from glob import glob
import platform
from sys import argv

PLUME_PATH = os.path.abspath(os.path.join(os.path.dirname(__file__), '../..'))

if (not which('clang')) and (not which('clang-cl')):
  print('Please install clang, clang-cl')
  exit(1)

args = ['-std=c11', '-Wall', '-Wextra', '-shared']
runtime_headers = PLUME_PATH + '/runtime/include'

output = PLUME_PATH + '/runtime/plume-vm.out'
library_output = PLUME_PATH + '/runtime/lib/libplume-library.o'
static_output = PLUME_PATH + '/runtime/lib/libplume-library.a'

compiler = 'clang' if which('clang') else 'clang-cl'

res = system(f'{compiler} -I{PLUME_PATH}/runtime/include {PLUME_PATH}/runtime/src/*.c {PLUME_PATH}/runtime/src/core/*.c -o {output} -g3')

library = system(f'{compiler} -o {library_output} -I{PLUME_PATH}/runtime/include {PLUME_PATH}/runtime/src/*.c {PLUME_PATH}/runtime/src/core/*.c -fPIC')
# static_library = system(f'ar r {static_output} {library_output}')

# print(library, static_library, res)

if res != 0:
  print('Failed to compile')
  exit(1)

print('Compiled successfully')