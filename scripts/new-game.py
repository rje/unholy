#!/usr/bin/env python3
"""Create an independent, playable Haskell game package from Circuit Breaker."""
import argparse
import json
from pathlib import Path
import re
import shutil
import subprocess

ROOT=Path(__file__).resolve().parents[1]
parser=argparse.ArgumentParser()
parser.add_argument('directory',type=Path)
parser.add_argument('--name',default='my-unholy-game')
parser.add_argument('--engine-local',type=Path,help='Use a local engine package instead of the published Git revision')
args=parser.parse_args()
if not re.fullmatch(r'[a-z][a-z0-9]*(?:-[a-z0-9]+)*',args.name):
    parser.error('name must be a lower-case Cabal package name')
destination=args.directory.resolve()
if destination.exists():
    parser.error('destination already exists')
template=ROOT/'examples/circuit-breaker'
destination.mkdir(parents=True)
shutil.copytree(template/'src',destination/'src')
shutil.copytree(template/'test',destination/'test')
cabal=(template/'circuit-breaker.cabal').read_text().replace('circuit-breaker',args.name)
(destination/(args.name+'.cabal')).write_text(cabal)
configuration='packages: .\nallow-newer: OpenGLRaw:base, OpenGLRaw:containers\npackage lua\n  flags: -system-lua -allow-unsafe-gc\n'
if args.engine_local:
    configuration=configuration.replace('packages: .',f'packages: .\n          {json.dumps(str(args.engine_local.resolve()))}')
else:
    revision=subprocess.check_output(['git','rev-parse','HEAD'],cwd=ROOT,text=True).strip()
    configuration+=f'\nsource-repository-package\n  type: git\n  location: https://github.com/rje/unholy.git\n  tag: {revision}\n  subdir: .\n'
(destination/'cabal.project').write_text(configuration)
scene=json.loads((ROOT/'projects/circuit-breaker/project.hj.json').read_text())
scene['name']=args.name
(destination/'project').mkdir()
(destination/'project/assets').mkdir()
(destination/'project/project.hj.json').write_text(json.dumps(scene,indent=2)+'\n')
shutil.copyfile(ROOT/'LICENSE',destination/'LICENSE')
(destination/'.gitignore').write_text('dist-newstyle/\nproject/dist/\n*.recovery\n')
(destination/'README.md').write_text(f'''# {args.name}

A separate Haskell game package using Unholy. Game source, scene and configuration
belong to this project. The starter is a complete switch-and-door puzzle.

```sh
brew install ghc cabal-install pkg-config raylib bullet
cabal update
cabal run {args.name} -- project/project.hj.json
cabal test {args.name}-tests --test-show-details=direct
cabal run {args.name} -- --build project/project.hj.json
```

Select an entity and open the Game inspector to edit player speed, a switch's
target, or door speed/travel. Save, reload and Play preserve these typed fields.
WASD moves, Space jumps, E activates a blue pressure pad, and R restarts.
Edit src/CircuitBreaker.hs and rebuild to change Haskell systems. The engine
library has no dependency on this game. Runtime GHC compilation is not required.
''')
print(f'Created {destination}\nRun: cd {destination}\ncabal run {args.name} -- project/project.hj.json')
