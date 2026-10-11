#!/usr/bin/env python3
"""Real editor policy persistence and a matched, relocated static glTF export."""
import json, shutil, subprocess
from pathlib import Path
from PIL import Image,ImageChops

ROOT=Path(__file__).resolve().parents[1]
BIN=ROOT/'build/unholy'
OUT=ROOT/'build/verification/imported-materials'
if OUT.exists():shutil.rmtree(OUT)
OUT.mkdir(parents=True)
shutil.copytree(ROOT/'projects/material-gallery/assets',OUT/'model')
scene=json.loads((ROOT/'projects/material-gallery/project.hj.json').read_text())
scene['entities'][0]['mesh']['geometry']='model/reliquaries.gltf'
project=OUT/'project.hj.json'
project.write_text(json.dumps(scene,indent=2)+'\n')
events=[(10,7,1350,586,0,0),(11,6,0,0,0,0),(14,5,0,0,0,0),
        (30,2,343,0,0,0),(31,2,83,0,0,0),(32,1,83,0,0,0),(33,1,343,0,0,0)]
replay=OUT/'material-policy.rae'
replay.write_text('# Imported material policy toggle and save.\nc '+str(len(events))+'\n'+''.join('e '+' '.join(map(str,e))+'\n' for e in events))
def run(*args,cwd=ROOT):
    subprocess.run(list(map(str,args)),cwd=cwd,check=True)
for expected in [False,True]:
    run(BIN,project,'--replay',replay,'--frames','60','--capture',OUT/f'editor-policy-{expected}.png')
    current=json.loads(project.read_text())
    assert current['entities'][0]['mesh']['importMaterials']==expected,'Actual editor did not persist the selected policy'
run(BIN,'--check',project)
run(BIN,'--play',project,'--frames','10','--capture',OUT/'local-player.png')
run(BIN,'--build',project)
export=OUT/'dist/material-gallery'
for name in ['reliquaries.gltf','reliquaries.bin','bronze.png','metal-rough.png','ridges.png']:
    assert (export/'model'/name).is_file(),f'Export missing {name}'
run(export/'unholy-player','--play',export/'project.hj.json','--frames','10','--capture',OUT/'export-player.png',cwd='/tmp')
local=Image.open(OUT/'local-player.png').convert('RGB')
relocated=Image.open(OUT/'export-player.png').convert('RGB')
assert local.size==relocated.size and ImageChops.difference(local,relocated).getbbox() is None,'Relocated player differs from the local rendered asset'
print('PASS actual editor import/override policy save/reload; top-level glTF and dependencies export; relocated player pixels match')
