from os import system
from shutil import which
import os.path
from glob import glob
import platform

# Check for Cabal and XMake 
if not which('cabal') or not which('xmake'):
  print('Please install cabal and xmake')
  exit(1)

# Check for submodules
if not os.path.isdir('runtime'):
  print('Please initialize submodules')
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

# Build the runtime project

runtime_executable = f"plume-vm{ext}"
runtime_executable_out = f"plume{ext}"

def build_xmake(project: str):
  system(f'cd {project}')
  system('xmake f --root -y')
  system('xmake --root')
  system('cd ..')

build_xmake('runtime')
system(f"cp runtime/bin/{runtime_executable} bin/{runtime_executable_out}")
build_xmake('standard')

print('Build ran successfully')