#!/usr/bin/env python3
"""Exercise persistent game fields and one-gesture undo in the real editor."""
import json
from pathlib import Path
import subprocess

ROOT=Path(__file__).resolve().parents[1]
OUT=ROOT/'build/verification/typed-authoring'
OUT.mkdir(parents=True,exist_ok=True)
events=[]
def click(frame,x,y):
    events.extend([f'e {frame} 7 {x} {y} 0 0',f'e {frame+1} 6 0 0 0 0',f'e {frame+4} 5 0 0 0 0'])
click(5,90,206)       # South assembly, then duplicate its linked switch and door.
click(15,1165,214)
click(25,90,240)      # Original South door.
click(35,1370,95)     # Game inspector.
events.extend(['e 45 7 1230 352 0 0','e 46 6 0 0 0 0','e 50 7 1290 352 0 0',
               'e 55 7 1360 352 0 0','e 60 5 0 0 0 0'])
events.extend(['e 70 2 341 0 0 0','e 71 2 90 0 0 0','e 72 1 90 0 0 0','e 74 1 341 0 0 0',
               'e 90 2 341 0 0 0','e 91 2 340 0 0 0','e 92 2 90 0 0 0','e 93 1 90 0 0 0',
               'e 94 1 340 0 0 0','e 95 1 341 0 0 0',
               'e 110 2 341 0 0 0','e 111 2 83 0 0 0','e 112 1 83 0 0 0','e 114 1 341 0 0 0'])
replay=OUT/'author-fields.rae'
replay.write_text('# Typed fields: clone linked assembly, drag speed, undo/redo/save.\nc '+str(len(events))+'\n'+'\n'.join(events)+'\n')
baseline=json.loads((ROOT/'projects/circuit-breaker/project.hj.json').read_text())
def run(frames,name):
    folder=OUT/name;folder.mkdir(exist_ok=True)
    path=folder/'project.hj.json';path.write_text(json.dumps(baseline))
    recovery=Path(str(path)+'.recovery');recovery.unlink(missing_ok=True)
    subprocess.run([str(ROOT/'build/unholy'),str(path),'--replay',str(replay),'--frames',str(frames),
                    '--capture',str(folder/'editor.png')],cwd=ROOT,check=True)
    return json.loads((path if frames>115 else recovery).read_text())
def speed(p): return next(e for e in p['entities'] if e['id']==4)['authored']['door']['speed']
edited=run(65,'edited');undone=run(85,'undone');saved=run(130,'saved')
assert speed(edited)>6, speed(edited)
assert speed(undone)==3, f'One undo did not restore the whole drag: {speed(undone)}'
assert abs(speed(saved)-speed(edited))<.001
assert len(saved['entities'])==len(baseline['entities'])+3
new_ids=[e['id'] for e in saved['entities'] if e['id']>=baseline['nextId']]
new_switch=next(e for e in saved['entities'] if e['id'] in new_ids and 'switch' in e.get('authored',{}))
assert new_switch['authored']['switch']['target']['entity'] in new_ids
subprocess.run([str(ROOT/'build/unholy'),'--check',str(OUT/'saved/project.hj.json')],cwd=ROOT,check=True)
# A structurally valid incomplete link remains openable for repair.
broken=json.loads(json.dumps(saved))
next(e for e in broken['entities'] if 'switch' in e.get('authored',{}))['authored']['switch']['target']['entity']=None
path=OUT/'incomplete.hj.json';path.write_text(json.dumps(broken))
subprocess.run([str(ROOT/'build/unholy'),str(path),'--frames','5'],cwd=ROOT,check=True)
check=subprocess.run([str(ROOT/'build/unholy'),'--check',str(path)],cwd=ROOT,capture_output=True,text=True)
assert check.returncode!=0 and 'needs a door target' in check.stdout
print(f'PASS real editor: persistent speed={speed(saved):.3f}, one undo/redo, linked assembly clone, save, and incomplete-link repair')
