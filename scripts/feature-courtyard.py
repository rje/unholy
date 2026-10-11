#!/usr/bin/env python3
"""Generate a self-contained, loadable showcase from original engine sample art."""
import json
import math
import shutil
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
OUT = ROOT / 'projects/feature-courtyard'
ART = OUT / 'assets'
ART.mkdir(parents=True, exist_ok=True)
for name in ['studio.hdr', 'ground.png', 'traveler.gltf', 'traveler.bin']:
    shutil.copyfile(ROOT / 'projects/threshold/assets' / name, ART / name)

scene = json.loads((ROOT / 'projects/material-lab/project.hj.json').read_text())
scene.update(name='Feature Courtyard', schemaVersion=2, haskellGame=None, entities=[])
scene['camera'] = dict(eye=[20, 16, 25], target=[0, 1, -2], fov=48)
scene['environment'].update(environmentMap='assets/studio.hdr', environmentIntensity=.8,
    environmentRotation=35, sunPower=2, sunShadows=True, fogDensity=.008,
    skyTop=[.035, .06, .095], skyBottom=[.12, .2, .25])


def add(name, geometry, pos, size=(1, 1, 1), color=(.18, .27, .31), metal=.3, rough=.4, emission=0):
    entity = dict(id=len(scene['entities']) + 1, name=name,
        transform=dict(position=pos, rotation=[0, 0, 0], scale=size))
    if geometry:
        entity.update(mesh=dict(geometry=geometry, texture='', importMaterials=False),
            material=dict(albedo=color, metallic=metal, roughness=rough, emission=emission))
    scene['entities'].append(entity)
    return entity


def height(x, z):
    edge = min(1, max(0, (max(abs(x), abs(z)) - 8) / 5))
    return edge * (.8 + .5 * math.sin(x * .35) * math.cos(z * .28))


ground = add('Terrain / editable hills and heightfield', 'terrain', [0, 0, 0],
    color=(.32, .42, .27), metal=0, rough=.88)
ground['mesh']['texture'] = 'assets/ground.png'
ground['terrain'] = dict(width=65, depth=65, spacing=.5,
    heights=[round(height((x - 32) * .5, (z - 32) * .5), 6) for z in range(65) for x in range(65)])
ground['body'] = dict(shape='heightfield', mass=0, velocity=[0, 0, 0], restitution=.1, friction=.6)
add('Animation stage', 'cube', [0, .15, 0], [20, .3, 5], rough=.65)
for x, name, clip, phase, color in [
    (-7.5, 'Idle', 'idle', .5, [.95, .62, .2]),
    (-2.5, 'Walk / phase A', 'walk', 0, [.12, .65, .85]),
    (2.5, 'Walk / phase B', 'walk', .5, [.55, .25, .8]),
    (7.5, 'Jump / paused pose', 'jump', 0, [.9, .3, .15]),
]:
    add(name + ' / plinth', 'cylinder', [x, .3, 0], [3.3, .5, 3.3], metal=.65)
    rim = add(name + ' / illuminated rim', 'torus', [x, .83, 0], [3.1, 3.1, 3.1],
        color=color, metal=.6, rough=.2, emission=.6)
    rim['transform']['rotation'] = [90, 0, 0]
    actor = add(name + ' / select to edit Animator', 'assets/traveler.gltf', [x, 2.15, 0],
        [1.6, 1.6, 1.6], color=color, metal=.65, rough=.26)
    actor['transform']['rotation'] = [0, 20, 0]
    actor['animator'] = dict(clip=clip, time=phase, rate=1, loop=True, playing=clip != 'jump')

for i, rough in enumerate([.06, .23, .45, .68, .9]):
    x = (i - 2) * 3
    add(f'IBL / roughness {rough}', 'sphere', [x, 1.4, 5.8], [1.8, 1.8, 1.8],
        color=(.88, .7, .4), metal=1, rough=rough)
    add('Reflection row / plinth', 'cylinder', [x, 0, 5.8], [2.2, .5, 2.2], rough=.7)

# One shared cube/material batch with varied poses; this is a visual demo, not an FPS claim.
for row in range(8):
    for col in range(16):
        x, z = (col - 7.5) * 1.6, -7.8 - row * .85
        size = 1.5 + .7 * math.sin(col * .6 + row * .4)
        column = add(f'Instances / column {row * 16 + col + 1}', 'cube',
            [x, height(x, z) + size / 2, z], [.6, size, .6],
            color=(.12, .4, .45), metal=.7, rough=.27)
        column['transform']['rotation'] = [0, row * 9, 0]

ring = add('Lua / rotating sculpture', 'torus', [0, 2.6, -4.8], [3, 3, 3],
    color=(.85, .66, .25), metal=.9, rough=.18)
ring['script'] = 'assets/scripts/courtyard.lua'
scene['nextId'] = len(scene['entities']) + 1
(OUT / 'project.hj.json').write_text(json.dumps(scene, indent=2) + '\n')
print(f'Generated Feature Courtyard: {len(scene["entities"])} entities')
