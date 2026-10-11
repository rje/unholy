#!/usr/bin/env python3
"""Real Load/Play/Stop/Build and relocated Feature Courtyard player workflow."""
import hashlib
import json
import math
import subprocess
import time
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
OUT = ROOT / 'build/verification/demos'
APP = ROOT / 'build/Unholy.app/Contents/MacOS/Unholy'
PROJECT = ROOT / 'projects/feature-courtyard/project.hj.json'
OUT.mkdir(parents=True, exist_ok=True)


def run(binary, *args, log, cwd=ROOT):
    with (OUT / log).open('w') as output:
        subprocess.run([str(binary), *map(str, args)], cwd=cwd, check=True,
            stdout=output, stderr=subprocess.STDOUT)
    text = (OUT / log).read_text()
    assert 'Lua error' not in text and 'Script error' not in text, text


def includes(expected, actual):
    """Save/export adds default fields and serializes floats at engine precision."""
    if isinstance(expected, dict):
        return all((actual.get(key) is None if value is None else key in actual and includes(value, actual[key])) for key, value in expected.items())
    if isinstance(expected, list):
        return len(expected) == len(actual) and all(includes(a, b) for a, b in zip(expected, actual))
    if isinstance(expected, (int, float)) and not isinstance(expected, bool):
        return math.isclose(expected, actual, abs_tol=1e-5, rel_tol=1e-5)
    return expected == actual


before = hashlib.sha256(PROJECT.read_bytes()).digest()
scene = json.loads(PROJECT.read_text())
run(APP, '--check', PROJECT, log='check.log')
run(APP, '--simulate', 240, PROJECT, log='simulate.log')
# Discover the same alphabetical pages as the editor, including local projects.
projects = sorted((ROOT / 'projects').glob('*/project.hj.json'))
index = projects.index(PROJECT)
def click(frame, x, y):
    return [(frame, 7, x, y, 0, 0), (frame + 1, 6, 0, 0, 0, 0), (frame + 4, 5, 0, 0, 0, 0)]
events = click(10, 280, 35)
for page in range(index // 4):
    events += click(20 + page * 10, 943, 272)
select = 20 + (index // 4) * 10
events += click(select, 650, 235 + 68 + (index % 4) * 42 + 16)
play = select + 10
events += [(play, 2, 294, 0, 0, 0), (play + 1, 1, 294, 0, 0, 0),
    (play + 150, 2, 294, 0, 0, 0), (play + 151, 1, 294, 0, 0, 0)]
events += click(play + 170, 436, 35)
replay = OUT / 'load-play-build.rae'
replay.write_text('c ' + str(len(events)) + '\n' + ''.join('e ' + ' '.join(map(str, event)) + '\n' for event in events))
baseline = ROOT / 'projects/material-lab/project.hj.json'
started = time.time()
for frames, name in [(play + 60, 'editor-playing'), (play + 120, 'editor-later'), (play + 190, 'editor-stopped-built')]:
    run(APP, baseline, '--replay', replay, '--frames', frames, '--capture', OUT / (name + '.png'), log=name + '.log')
assert hashlib.sha256(PROJECT.read_bytes()).digest() == before, 'Play/Stop/Build modified the demo source'
export = PROJECT.parent / 'dist/feature-courtyard'
assert (export / 'project.hj.json').stat().st_mtime >= started, 'Load picker/Build did not create a fresh demo export'
assert includes(scene, json.loads((export / 'project.hj.json').read_text())), 'Export changed authored demo fields'
for asset in (PROJECT.parent / 'assets').rglob('*'):
    if asset.is_file():
        assert asset.read_bytes() == (export / asset.relative_to(PROJECT.parent)).read_bytes(), 'Export omitted/changed ' + asset.name
run(export / 'unholy-player', '--check', export / 'project.hj.json', cwd='/tmp', log='export-check.log')
run(export / 'unholy-player', '--play', export / 'project.hj.json', '--frames', 90,
    '--capture', OUT / 'export-player.png', cwd='/tmp', log='export-player.log')
print('PASS demo picker Load/Play/Stop/Build, unchanged source, authored export and relocated player; inspect captures')
