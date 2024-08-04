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
ffi_files = glob(PLUME_PATH + '/standard/c-ffi/**/*.c', recursive=True)
runtime_headers = PLUME_PATH + '/runtime/include'
runtime_library = [PLUME_PATH + '/runtime/lib/libplume-library.a', 'curl']

output = PLUME_PATH + '/standard/native.plmc'

compiler = 'clang' if which('clang') else 'clang-cl'

res = system(f'{compiler}  -I{PLUME_PATH}/runtime/include {PLUME_PATH}/standard/c-ffi/*.c {PLUME_PATH}/runtime/lib/libplume-library.a -lcurl -shared -o {output}')

if res != 0:
  print('Failed to compile')
  exit(1)

print('Compiled successfully')