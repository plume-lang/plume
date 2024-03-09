from utility.formatter import fourmolu
from utility.linter import run_linter
from glob import glob

def flatten[a](xss: list[list[a]]) -> list[a]:
  if xss == []: return []
  else: return xss[0] + flatten(xss[1:])

def main() -> None:
  extensions = ['hs', 'lhs']
  folders = ['src', 'app', 'test']

  # Get all Haskell files in the src directory
  files = flatten([
    glob(f"{folder}/**/*.{ext}", recursive=True)
      for folder in folders 
        for ext in extensions])
    
  # Run the linter on each file
  for (i, file) in enumerate(files): 
    run_linter(file, (i + 1, len(files)))
  
  fourmolu('.')  

  print('Linter ran successfully')

if __name__ == '__main__': main()
  