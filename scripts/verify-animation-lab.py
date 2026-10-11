#!/usr/bin/env python3
"""Bounded real-editor clip choice, paused authoring and save/reopen check."""
import json
import shutil
import subprocess
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
OUT = ROOT / 'build/verification/animation-lab-authoring'
APP = ROOT / 'build/Unholy.app/Contents/MacOS/Unholy'
OUT.mkdir(parents=True, exist_ok=True)
shutil.copytree(ROOT / 'projects/animation-lab/assets', OUT / 'assets', dirs_exist_ok=True)
scene = json.loads((ROOT / 'projects/animation-lab/project.hj.json').read_text())
actor = next(e for e in scene['entities'] if e.get('mesh', {}).get('geometry', '').endswith('fox-with-tag.gltf'))
actor.update(id=1, name='Fox / rigid head tag')
actor['transform'].update(position=[0, 0, 0], rotation=[0, 0, 0])
scene.update(name='Fox clip authoring', entities=[actor], nextId=2)
scene['camera'] = dict(eye=[3, 2, 5], target=[0, 1, 0], fov=45)
project = OUT / 'project.hj.json'
project.write_text(json.dumps(scene, indent=2) + '\n')


def run(*args, log):
    with (OUT / log).open('w') as output:
        subprocess.run([str(APP), *map(str, args)], cwd=ROOT, check=True,
            stdout=output, stderr=subprocess.STDOUT)
    text = (OUT / log).read_text()
    assert 'Lua error' not in text and 'Script error' not in text, text


def click(frame, x, y):
    return [(frame, 7, x, y, 0, 0), (frame + 1, 6, 0, 0, 0, 0), (frame + 4, 5, 0, 0, 0, 0)]


def replay(name, events, frames=75):
    path = OUT / (name + '.rae')
    events.sort()
    path.write_text('c ' + str(len(events)) + '\n' + ''.join(
        'e ' + ' '.join(map(str, event)) + '\n' for event in events))
    run(project, '--replay', path, '--frames', frames,
        '--capture', OUT / (name + '.png'), log=name + '.log')
    return json.loads(project.read_text())


# Scroll 1160px: Next clip appears at 507, Clamp at 620.
scroll = [(10, 7, 1300, 600, 0, 0), (11, 8, 0, -29, 0, 0)]
save = [(55, 2, 343, 0, 0, 0), (56, 2, 83, 0, 0, 0),
        (57, 1, 83, 0, 0, 0), (58, 1, 343, 0, 0, 0)]
edited = replay('editor-authored', scroll + click(20, 1340, 507) + click(30, 1175, 620) + save)
config = edited['entities'][0]['animator']
assert config['clip'] == 'Walk' and config['time'] == 0, 'Next clip failed to choose/reset: ' + str(config)
assert not config['playing'] and not config['loop'], 'Paused/clamped authoring failed: ' + str(config)
reopened = replay('editor-reopened', scroll)
assert reopened['entities'][0]['animator'] == config, 'Reopen lost authored animator'
run('--check', project, log='check.log')
print('PASS real Fox clip choice/reset, paused/clamped authored pose, save/reopen and preflight; inspect captures')
