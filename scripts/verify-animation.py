#!/usr/bin/env python3
"""Actual animator inspector edits, save/reopen and runtime pose captures."""
import json,shutil,subprocess
from pathlib import Path
ROOT=Path(__file__).resolve().parents[1]
OUT=ROOT/'build/verification/animation';OUT.mkdir(parents=True,exist_ok=True)
APP=ROOT/'build/Unholy.app/Contents/MacOS/Unholy'
shutil.copytree(ROOT/'projects/threshold/assets',OUT/'assets',dirs_exist_ok=True)
scene=json.loads((ROOT/'projects/threshold/project.hj.json').read_text())
scene.update(name='Animation authoring',haskellGame=None,nextId=2,entities=[scene['entities'][1]])
scene['entities'][0].update(id=1,name='Animated traveler')
scene['entities'][0]['animator']['time']=.5
scene['entities'][0].pop('body');scene['entities'][0].pop('authored')
scene['entities'][0]['transform']['position']=[0,1,0]
scene['camera']={'eye':[0,2,6],'target':[0,1,0],'fov':45}
project=OUT/'project.hj.json';project.write_text(json.dumps(scene,indent=2)+'\n')
def run(*args,log='editor.log'):
    with (OUT/log).open('w') as output:subprocess.run([str(APP),*map(str,args)],cwd=ROOT,stdout=output,stderr=subprocess.STDOUT,check=True)
def replay(name,events,frames=70):
    path=OUT/(name+'.rae');events.sort()
    path.write_text('c '+str(len(events))+'\n'+''.join('e '+' '.join(map(str,e))+'\n' for e in events))
    run(project,'--replay',path,'--frames',frames,'--capture',OUT/(name+'.png'),log=name+'.log')
    return json.loads(project.read_text())
# Scroll the inspector by 1160px. Animator clip/time/rate appear at 507/547.
base=[(10,7,1300,600,0,0),(11,8,0,-29,0,0)]
# Pause and clamp through real buttons; retain the authored pose and rate.
events=base+[(20,7,1340,620,0,0),(21,6,0,0,0,0),(24,5,0,0,0,0),
 (28,7,1175,620,0,0),(29,6,0,0,0,0),(32,5,0,0,0,0),
 (55,2,343,0,0,0),(56,2,83,0,0,0),(57,1,83,0,0,0),(58,1,343,0,0,0)]
edited=replay('editor-authored',events)
a=edited['entities'][0]['animator']
assert a['time']==.5 and not a['playing'] and not a['loop'], 'Animator inspector did not persist pose/pause/clamp: '+str(a)
replay('editor-reopened',base)
run('--check',project,log='check.log')
# Sample game's own runtime selects the walk clip; two phases show articulation.
route=ROOT/'build/verification/threshold/editor.rae'
run(ROOT/'projects/threshold/project.hj.json','--replay',route,'--frames',90,'--capture',OUT/'editor-walk.png',log='walk.log')
run(ROOT/'projects/threshold/project.hj.json','--replay',route,'--frames',260,'--capture',OUT/'editor-jump.png',log='jump.log')
print('PASS animator inspector pause/clamp and retained pose, save/reopen, preflight and runtime pose captures; inspect images')
