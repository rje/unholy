#!/usr/bin/env python3
"""Actual editor HDR attachment, persistence and relocated-player parity."""
import json
import shutil
import subprocess
from pathlib import Path
from PIL import Image, ImageChops

ROOT = Path(__file__).resolve().parents[1]
OUT = ROOT/'build/verification/environment'
APP = ROOT/'build/Unholy.app/Contents/MacOS/Unholy'
OUT.mkdir(parents=True, exist_ok=True)
(OUT/'assets').mkdir(exist_ok=True)
shutil.copyfile(ROOT/'projects/material-gallery/assets/studio.hdr', OUT/'assets/studio.hdr')
scene = json.loads((ROOT/'projects/material-lab/project.hj.json').read_text())
scene.update(name='Environment check', nextId=2, haskellGame=None, entities=[{
    'id': 1, 'name': 'Reflective sphere',
    'transform': {'position': [0, 1, 0], 'rotation': [0, 0, 0], 'scale': [2, 2, 2]},
    'mesh': {'geometry': 'sphere', 'texture': ''},
    'material': {'albedo': [.8, .7, .5], 'metallic': .85, 'roughness': .22, 'emission': 0}}])
scene['environment'].update(environmentMap='', environmentIntensity=8, environmentRotation=270, sunPower=0, fogDensity=0)
scene['camera'] = {'eye': [0, 2, 6], 'target': [0, 1, 0], 'fov': 45}
project = OUT/'project.hj.json'
project.write_text(json.dumps(scene, indent=2)+'\n')


def run(*args, cwd=ROOT):
    subprocess.run([str(APP), *map(str, args)], cwd=cwd, check=True)


def replay(name, events):
    path = OUT/(name+'.rae')
    path.write_text('c '+str(len(events))+'\n'+''.join('e '+' '.join(map(str, event))+'\n' for event in events))
    run(project, '--replay', path, '--frames', 70, '--capture', OUT/(name+'.png'))
    return json.loads(project.read_text())


save = [(50, 2, 343, 0, 0, 0), (51, 2, 83, 0, 0, 0), (52, 1, 83, 0, 0, 0), (53, 1, 343, 0, 0, 0)]
attached = replay('editor-attached', [(10, 7, 90, 730, 0, 0), (11, 6, 0, 0, 0, 0), (14, 5, 0, 0, 0, 0),
    (20, 7, 1300, 600, 0, 0), (21, 8, 0, -20, 0, 0), (30, 7, 1280, 460, 0, 0)] + save)
assert attached['environment']['environmentMap'] == 'assets/studio.hdr', 'HDR asset click did not attach the environment'
assert attached['environment']['environmentIntensity'] == 8 and attached['environment']['environmentRotation'] == 270, 'Hovering the inspector clamped saved values'
# Reopen, scroll World inspector and edit rotation with the real slider.
edited = replay('editor-rotated', [(10, 7, 1270, 95, 0, 0), (11, 6, 0, 0, 0, 0), (14, 5, 0, 0, 0, 0),
    (20, 7, 1300, 600, 0, 0), (21, 8, 0, -20, 0, 0), (30, 7, 1280, 507, 0, 0), (31, 6, 0, 0, 0, 0), (34, 5, 0, 0, 0, 0)] + save)
assert edited['environment']['environmentRotation'] != 270, 'World rotation slider did not persist its edit'
assert edited['environment']['environmentIntensity'] == 8, 'Rotation edit changed intensity'
run('--check', project)
run('--play', project, '--frames', 10, '--capture', OUT/'local-player.png')
run('--build', project)
export = OUT/'dist/environment-check'
assert (export/'assets/studio.hdr').read_bytes() == (OUT/'assets/studio.hdr').read_bytes()
subprocess.run([str(export/'unholy-player'), '--play', str(export/'project.hj.json'), '--frames', '10', '--capture', str(OUT/'export-player.png')], cwd='/tmp', check=True)
local = Image.open(OUT/'local-player.png').convert('RGB')
relocated = Image.open(OUT/'export-player.png').convert('RGB')
assert ImageChops.difference(local, relocated).getbbox() is None, 'Relocated HDR player pixels differ'
print('PASS actual editor HDR attachment, preserved authored range, rotation/save/reopen, exported asset and relocated player pixel parity')
