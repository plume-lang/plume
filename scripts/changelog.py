from os import system, getcwd
from shutil import which
from pathlib import Path

if not which('auto-changelog'):
  print('auto-changelog is not installed. Please run `npm install -g auto-changelog` to install it.')
  exit(1)

cwd = getcwd()

breaking_pattern = '^feat.*'
issue_pattern = '^fix.*'
backfill_limit = 10
template = Path(cwd) / 'docs' / 'changelog.hbs'

print('Generating changelog with the following parameters:')
print(f' - Breaking pattern: {breaking_pattern}')
print(f' - Issue pattern: {issue_pattern}')
print(f' - Backfill limit: {backfill_limit}')
print(f' - Template: {template}')

system(f"""
auto-changelog \\
  --breaking-pattern {breaking_pattern} \\
  --issue-pattern {issue_pattern} \\
  --backfill-limit {backfill_limit} -t {template} \\
  > /dev/null 2>&1
""")

print('Changelog generated successfully!')