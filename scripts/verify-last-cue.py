#!/usr/bin/env python3
"""Qualify the research-led sample through real Load/Play/Stop/Build and export."""
import hashlib
import json
import os
import subprocess
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
OUT = ROOT/'build/verification/last-cue'
APP = ROOT/'build/Unholy.app/Contents/MacOS/Unholy'
SCENE = ROOT/'projects/last-cue/project.hj.json'
OUT.mkdir(parents=True, exist_ok=True)
env = dict(os.environ, UNHOLY_RECORD_ROUTE=str(OUT/'route.rae'), UNHOLY_WINDOW_HIDDEN='1')
subprocess.run(['cabal', 'test', 'last-cue-tests', '--test-show-details=direct'], cwd=ROOT, env=env, check=True,
               stdout=(OUT/'route-test.log').open('w'), stderr=subprocess.STDOUT)
route = [tuple(map(int,line.split()[1:])) for line in (OUT/'route.rae').read_text().splitlines() if line.startswith('e ')]
frames = int((OUT/'route.rae.frames').read_text())

def replay(name, events):
    path = OUT/(name+'.rae')
    path.write_text('c '+str(len(events))+'\n'+''.join('e '+' '.join(map(str,e))+'\n' for e in sorted(events)))
    return path

def click(frame, x, y):
    return [(frame,7,x,y,0,0),(frame+1,6,0,0,0,0),(frame+4,5,0,0,0,0)]

def run(binary, *args, name, namespace, cwd=ROOT):
    with (OUT/(name+'.log')).open('w') as log:
        subprocess.run([str(binary),*map(str,args)], cwd=cwd, check=True,
            env=dict(os.environ,UNHOLY_SAVE_DIR=str(OUT/namespace),UNHOLY_WINDOW_HIDDEN='1'), stdout=log, stderr=subprocess.STDOUT)
    output=(OUT/(name+'.log')).read_text()
    assert 'initialization failed' not in output and 'preflight failed' not in output and 'Script error' not in output, output
    return output

projects = sorted((ROOT/'projects').glob('*/project.hj.json'))
index = projects.index(SCENE)
events = click(10,280,35)
for page in range(index//4):
    events += click(20+page*10,943,272)
select = 20+(index//4)*10
events += click(select,650,235+68+(index%4)*42+16)
play = select+10
events += [(play,2,294,0,0,0),(play+1,1,294,0,0,0)]
events += [(n+play,*other) for n,*other in route]
complete = frames+play
editor_route = replay('editor-route',events)
before = hashlib.sha256(SCENE.read_bytes()).digest()
output = run(APP,ROOT/'projects/material-lab/project.hj.json','--replay',editor_route,'--frames',complete,
    '--capture',OUT/'editor-complete.png',name='editor-complete',namespace='editor-saves')
# Editor logs are drawn inside its console; inspect the screenshot for completion.
events += [(complete+10,2,294,0,0,0),(complete+11,1,294,0,0,0)]
events += click(complete+25,436,35)
stop = replay('editor-stop-build',events)
run(APP,ROOT/'projects/material-lab/project.hj.json','--replay',stop,'--frames',complete+50,
    '--capture',OUT/'editor-stopped-built.png',name='editor-stopped-built',namespace='editor-saves')
assert hashlib.sha256(SCENE.read_bytes()).digest()==before, 'Play/Stop changed authored project'
export = SCENE.parent/'dist/last-cue'
for manifest in ['project.hj.json','dock.hj.json','flies.hj.json']:
    assert json.loads((export/manifest).read_text())==json.loads((SCENE.parent/manifest).read_text()), manifest
for asset in (SCENE.parent/'assets').rglob('*'):
    if asset.is_file():
        assert asset.read_bytes()==(export/asset.relative_to(SCENE.parent)).read_bytes(), asset
run(export/'unholy-player','--check',export/'project.hj.json',name='export-check',namespace='export-saves',cwd='/tmp')
result = run(export/'unholy-player','--play',export/'project.hj.json','--replay',OUT/'route.rae','--frames',frames,
    '--capture',OUT/'export-complete.png',name='export-complete',namespace='export-saves',cwd='/tmp')
assert 'Curtain call - both acts complete' in result, result
resume = replay('continue-pause',[(10,2,264,0,0,0),(11,1,264,0,0,0),(12,2,257,0,0,0),(13,1,257,0,0,0),(25,2,256,0,0,0),(26,1,256,0,0,0)])
result = run(export/'unholy-player','--play',export/'project.hj.json','--replay',resume,'--frames',60,
    '--capture',OUT/'export-continue-paused.png',name='export-continue-paused',namespace='export-saves',cwd='/tmp')
assert 'Entered level: flies' in result, result
run(APP,'--play',SCENE,'--replay',OUT/'route.rae','--frames',120,
    '--capture',OUT/'scene-dock.png',name='scene-dock',namespace='beauty-saves')
run(APP,'--play',SCENE,'--replay',OUT/'route.rae','--frames',1150,
    '--capture',OUT/'fly-tower.png',name='fly-tower',namespace='beauty-saves')
print('PASS Load/Play/complete/Stop/Build; unchanged scene; relocated two-act completion; fresh-process Continue. Inspect captures.')
