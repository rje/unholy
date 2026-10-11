#!/usr/bin/env python3
"""Actual Add terrain, preset, undo/redo, save/reopen and export workflow."""
import json,shutil,subprocess
from pathlib import Path
ROOT=Path(__file__).resolve().parents[1]
OUT=ROOT/'build/verification/terrain';OUT.mkdir(parents=True,exist_ok=True)
APP=ROOT/'build/Unholy.app/Contents/MacOS/Unholy'
shutil.copytree(ROOT/'projects/threshold/assets',OUT/'assets',dirs_exist_ok=True)
scene=json.loads((ROOT/'projects/threshold/project.hj.json').read_text())
scene.update(name='Terrain authoring',haskellGame=None,nextId=1,entities=[])
project=OUT/'project.hj.json'
# Add modal centered at (410,235); terrain is row 6 at y=498.
events=[(10,7,175,95,0,0),(11,6,0,0,0,0),(14,5,0,0,0,0),
 (20,7,650,510,0,0),(21,6,0,0,0,0),(24,5,0,0,0,0),
 (30,7,1300,600,0,0),(31,8,0,-38,0,0),
 (40,7,1340,465,0,0),(41,6,0,0,0,0),(44,5,0,0,0,0),
 (50,2,343,0,0,0),(51,2,90,0,0,0),(52,1,90,0,0,0),(53,1,343,0,0,0),
 (70,2,343,0,0,0),(71,2,340,0,0,0),(72,2,90,0,0,0),(73,1,90,0,0,0),(74,1,340,0,0,0),(75,1,343,0,0,0),
 (86,7,90,788,0,0),(87,6,0,0,0,0),(90,5,0,0,0,0),
 (100,2,343,0,0,0),(101,2,83,0,0,0),(102,1,83,0,0,0),(103,1,343,0,0,0)]
path=OUT/'authoring.rae';path.write_text('c '+str(len(events))+'\n'+''.join('e '+' '.join(map(str,e))+'\n' for e in events))
def run(*args,cwd=ROOT,log='run.log',binary=APP):
    with (OUT/log).open('w') as output:subprocess.run([str(binary),*map(str,args)],cwd=cwd,check=True,stdout=output,stderr=subprocess.STDOUT)
def execute(frames,name):
    project.write_text(json.dumps(scene,indent=2)+'\n')
    recovery=Path(str(project)+'.recovery');recovery.unlink(missing_ok=True)
    run(project,'--replay',path,'--frames',frames,'--capture',OUT/(name+'.png'),log=name+'.log')
    return json.loads((project if frames>103 else recovery).read_text())
undo=execute(60,'editor-undone')
assert len(undo['entities'])==1 and set(undo['entities'][0]['terrain']['heights'])=={0}, 'Undo did not restore flat terrain'
saved=execute(115,'editor-authored');grid=saved['entities'][0]['terrain']
assert grid['width']==33 and grid['depth']==33 and max(grid['heights'])>.5, 'Add/preset/redo did not persist terrain'
assert saved['entities'][0]['mesh']['texture']=='assets/ground.png', 'Texture attachment lost terrain'
assert saved['entities'][0]['body']['shape']=='heightfield', 'Add terrain omitted the heightfield collider'
# Reopen with the real inspector scrolled to the finite grid.
scroll=OUT/'reopen.rae';scroll.write_text('c 2\ne 10 7 1300 600 0 0\ne 11 8 0 -38 0 0\n')
run(project,'--replay',scroll,'--frames',30,'--capture',OUT/'editor-reopened.png',log='reopen.log')
run('--check',project,log='check.log');run('--build',project,log='build.log')
export=OUT/'dist/terrain-authoring'
assert json.loads((export/'project.hj.json').read_text())==saved, 'Export omitted edited terrain samples'
run('--play',export/'project.hj.json','--frames',10,'--capture',OUT/'exported-terrain.png',cwd='/tmp',binary=export/'unholy-player',log='export.log')
# A model asset converts terrain coherently instead of retaining a stale grid.
converted=OUT/'model-attach.hj.json';converted.write_text(json.dumps(saved))
attach=OUT/'attach.rae';click=[(10,7,90,846,0,0),(11,6,0,0,0,0),(14,5,0,0,0,0),(30,2,343,0,0,0),(31,2,83,0,0,0),(32,1,83,0,0,0),(33,1,343,0,0,0)]
attach.write_text('c '+str(len(click))+'\n'+''.join('e '+' '.join(map(str,e))+'\n' for e in click))
run(converted,'--replay',attach,'--frames',45,'--capture',OUT/'editor-model-attachment.png',log='attachment.log')
model=json.loads(converted.read_text())['entities'][0]
assert 'terrain' not in model and model['body']['shape']=='box' and model['mesh']['geometry']=='assets/ramp.gltf', 'Model attachment retained incompatible terrain'
run('--check',converted,log='attachment-check.log')
print('PASS Add terrain, Hills preset, undo/redo, save/reopen, heightfield persistence and relocated export; inspect images')
