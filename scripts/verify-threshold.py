#!/usr/bin/env python3
"""Replay Threshold through the app and an exported player; inspect HUD captures."""
import hashlib
import json
import subprocess
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
OUT = ROOT/'build/verification/threshold'
APP = ROOT/'build/Unholy.app/Contents/MacOS/Unholy'
SCENE = ROOT/'projects/threshold/project.hj.json'
OUT.mkdir(parents=True, exist_ok=True)


def replay(name, events):
    path = OUT/(name+'.rae')
    events.sort()
    path.write_text('# Threshold route.\nc '+str(len(events))+'\n'+''.join('e '+' '.join(map(str, event))+'\n' for event in events))
    return path


keyboard = [(10, 2, 294, 0, 0, 0), (11, 1, 294, 0, 0, 0), (20, 2, 87, 0, 0, 0)]
for frame in range(180, 601, 50):
    keyboard += [(frame, 2, 32, 0, 0, 0), (frame+12, 1, 32, 0, 0, 0)]
keyboard += [(650, 1, 87, 0, 0, 0), (660, 2, 82, 0, 0, 0), (661, 1, 82, 0, 0, 0), (700, 2, 294, 0, 0, 0), (701, 1, 294, 0, 0, 0)]
editor = replay('editor', keyboard)
controller = []
for frame in range(10, 631):
    controller += [(frame, 9, 0, 0, 0, 0), (frame, 13, 0, 1, -32768, 0)]
    controller.append((frame, 12 if frame >= 180 and (frame-180) % 50 < 12 else 11, 0, 7, 0, 0))
controller.append((631, 10, 0, 0, 0, 0))
pad = replay('controller', controller)


def run(binary, *args, cwd=ROOT, log):
    with (OUT/log).open('w') as output:
        subprocess.run([str(binary), *map(str, args)], cwd=cwd, check=True, stdout=output, stderr=subprocess.STDOUT)


before = hashlib.sha256(SCENE.read_bytes()).digest()
run(APP, SCENE, '--replay', editor, '--frames', 640, '--capture', OUT/'editor-complete.png', log='editor-complete.log')
run(APP, SCENE, '--replay', editor, '--frames', 690, '--capture', OUT/'editor-reset.png', log='editor-reset.log')
run(APP, SCENE, '--replay', editor, '--frames', 720, '--capture', OUT/'editor-stopped.png', log='editor-stopped.log')
assert hashlib.sha256(SCENE.read_bytes()).digest() == before, 'Play/Reset/Stop changed the authored scene'
run(APP, '--play', SCENE, '--replay', pad, '--frames', 650, '--capture', OUT/'controller-local.png', log='controller-local.log')
run(APP, '--build', SCENE, log='build.log')
export = ROOT/'projects/threshold/dist/threshold'
exported = json.loads((export/'project.hj.json').read_text())
assert exported == json.loads(SCENE.read_text()), 'Export changed the scene'
for asset in ['traveler.gltf', 'traveler.bin', 'ground.png', 'ramp.gltf', 'ramp.bin', 'studio.hdr', 'audio/jump.wav', 'audio/pickup.wav']:
    assert (export/'assets'/asset).is_file(), 'Export omitted '+asset
run(export/'unholy-player', '--play', export/'project.hj.json', '--replay', pad, '--frames', 650, '--capture', OUT/'controller-export.png', cwd='/tmp', log='controller-export.log')
print('PASS real editor/player replays, unchanged authored scene and relocated export; inspect captures for completed/reset HUDs')
