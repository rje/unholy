#!/usr/bin/env python3
"""Exercise the real editor on a disposable project; requires a desktop display."""
import json
import subprocess
from pathlib import Path
ROOT = Path(__file__).resolve().parents[1]
OUT = ROOT / 'build/verification/unholy-runtime'
FIXTURE = OUT / 'editor-fixture'

def fixture():
    project = json.loads((ROOT / 'projects/material-lab/project.hj.json').read_text())
    project.update(name='Unholy authoring check', nextId=3, entities=[
        dict(id=1, name='Assembly', transform=dict(position=[0,.5,0], rotation=[0,0,0], scale=[1,1,1]),
             mesh=dict(geometry='cube', texture=''), material=dict(albedo=[.2,.6,.9], metallic=.4, roughness=.3, emission=0),
             body=dict(shape='box', mass=0, velocity=[0,0,0], restitution=.1, friction=.5)),
        dict(id=2, name='Trim', transform=dict(position=[0,1.2,0], rotation=[0,0,0], scale=[.25,.25,.25]),
             mesh=dict(geometry='sphere', texture=''), material=dict(albedo=[1,.6,.2], metallic=.8, roughness=.2, emission=0),
             parent=dict(entity=1, local=dict(position=[0,.7,0], rotation=[0,0,0], scale=[.25,.25,.25])))])
    asset_dir = FIXTURE / 'assets/prefabs'
    asset_dir.mkdir(parents=True, exist_ok=True)
    for old in asset_dir.glob('*.json'):
        old.unlink()
    (FIXTURE / 'project.hj.json').write_text(json.dumps(project, indent=2))
    prefab = dict(project, prefabRoot=1)
    prefab['entities'][0]['transform']['position'] = [0,0,0]
    (asset_dir / 'example.prefab.json').write_text(json.dumps(prefab, indent=2))

def run(frames, name):
    subprocess.run([str(ROOT / 'build/Unholy.app/Contents/MacOS/Unholy'), str(FIXTURE / 'project.hj.json'),
                    '--replay', str(ROOT / 'test/authoring-smoke.rae'), '--frames', str(frames),
                    '--capture', str(OUT / (name+'.png'))], cwd=ROOT, check=True)
    path = FIXTURE / ('project.hj.json.recovery' if frames<140 else 'project.hj.json')
    return json.loads(path.read_text())

if __name__ == '__main__':
    fixture()
    undone = run(48, 'authoring-undo')
    assert undone['entities'][0]['transform']['position'] == [0,.5,0], 'Drag undo did not restore the assembly'
    redone = run(60, 'authoring-redo')
    x = redone['entities'][0]['transform']['position'][0]
    assert .5 < x < 1.5, 'Redo did not restore the gizmo edit'
    assert abs(redone['entities'][1]['transform']['position'][0]-x)<.001, 'Child did not follow its parent'
    saved = run(160, 'authoring-after')
    assert len(saved['entities']) == 4 and saved['entities'][2]['id'] == 3, 'Prefab placement did not create both entities'
    assert saved['entities'][3]['parent']['entity'] == 3, 'Prefab parent IDs were not remapped'
    prefab = json.loads((FIXTURE / 'assets/prefabs/assembly.prefab.json').read_text())
    assert prefab['prefabRoot'] == 1 and len(prefab['entities']) == 2, 'Prefab save lost the assembly'
    print('PASS real editor: gizmo drag, one undo/redo, child transforms, prefab save/place and project save')
