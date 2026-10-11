#!/usr/bin/env python3
"""Stage the pinned, attributed Fox rig and add an original rigid joint tag."""
import copy
import json
import math
import shutil
import struct
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
OUT = ROOT / 'projects/animation-lab'
ART = OUT / 'assets'
FOX = ART / 'fox'
ART.mkdir(parents=True, exist_ok=True)
for name in ['studio.hdr', 'ground.png']:
    shutil.copyfile(ROOT / 'projects/threshold/assets' / name, ART / name)

# Keep the downloaded Fox files unchanged. A separate variant adds our gold tag.
rig = json.loads((FOX / 'Fox.gltf').read_text())
variant = copy.deepcopy(rig)
points = [(-3,-3,-3),(3,-3,-3),(3,3,-3),(-3,3,-3),(-3,-3,3),(3,-3,3),(3,3,3),(-3,3,3)]
indices = [0,2,1,0,3,2,4,5,6,4,6,7,0,4,7,0,7,3,1,2,6,1,6,5,3,7,6,3,6,2,0,1,5,0,5,4]
encoded = b''.join(struct.pack('<3f', *points[i]) for i in indices)
(FOX / 'tag.bin').write_bytes(encoded)
buffer_index = len(variant['buffers'])
variant['buffers'].append(dict(uri='tag.bin', byteLength=len(encoded)))
view_index = len(variant['bufferViews'])
variant['bufferViews'].append(dict(buffer=buffer_index, byteOffset=0, byteLength=len(encoded)))
accessor_index = len(variant['accessors'])
variant['accessors'].append(dict(bufferView=view_index, componentType=5126,
    count=len(indices), type='VEC3', min=[-3,-3,-3], max=[3,3,3]))
material_index = len(variant['materials'])
variant['materials'].append(dict(name='Original gold joint tag', pbrMetallicRoughness=dict(
    baseColorFactor=[.9,.64,.14,1], metallicFactor=.85, roughnessFactor=.2)))
mesh_index = len(variant['meshes'])
variant['meshes'].append(dict(name='Rigid joint tag', primitives=[dict(
    attributes=dict(POSITION=accessor_index), material=material_index)]))
node_index = len(variant['nodes'])
variant['nodes'].append(dict(name='Unholy rigid head tag', mesh=mesh_index, translation=[4,0,0]))
variant['nodes'][8].setdefault('children', []).append(node_index)
(FOX / 'fox-with-tag.gltf').write_text(json.dumps(variant, separators=(',', ':')) + '\n')

scene = json.loads((ROOT / 'projects/material-lab/project.hj.json').read_text())
scene.update(name='Animation Lab', schemaVersion=2, haskellGame=None, entities=[])
scene['camera'] = dict(eye=[22,15,27], target=[0,2,-3], fov=48)
scene['environment'].update(environmentMap='assets/studio.hdr', environmentIntensity=.85,
    environmentRotation=30, sunPower=2.5, sunShadows=True, fogDensity=.006,
    skyTop=[.035,.06,.095], skyBottom=[.11,.19,.24])

def add(name, geometry, pos, size=(1,1,1), color=(.12,.2,.26), metal=.2, rough=.6):
    entity = dict(id=len(scene['entities'])+1, name=name,
        transform=dict(position=pos, rotation=[0,0,0], scale=size))
    if geometry:
        entity.update(mesh=dict(geometry=geometry, texture='', importMaterials=False),
            material=dict(albedo=color, metallic=metal, roughness=rough, emission=0))
    scene['entities'].append(entity)
    return entity

floor = add('Textured courtyard', 'plane', [0,0,0], [45,1,38], color=(.27,.36,.29), rough=.9)
floor['mesh']['texture'] = 'assets/ground.png'
add('Animation stage', 'cube', [0,.15,1], [31,.3,9])
for x, clip, phase, file, name, color in [
    (-11.1,'Survey',0,'Fox.gltf','Survey / original rig',(.94,.62,.2)),
    (-3.7,'Walk',0,'Fox.gltf','Walk / independent phase',(.12,.67,.87)),
    (3.7,'Run',.2,'Fox.gltf','Run / GPU skinning',(.62,.3,.87)),
    (11.1,'Survey',1,'fox-with-tag.gltf','Paused / rigid head tag',(.85,.32,.2))]:
    add(name+' plinth', 'cylinder', [x,.3,1], [6.6,.35,6.6], metal=.7, rough=.32)
    ring = add(name+' rim', 'torus', [x,.67,1], [6.2,6.2,6.2], color=color, metal=.7, rough=.2)
    ring['transform']['rotation'] = [90,0,0]
    ring['material']['emission'] = .45
    actor = add(name+' / select Animator', 'assets/fox/'+file, [x,.68,1], [.037]*3)
    actor['mesh']['importMaterials'] = True
    actor['transform']['rotation'] = [0,25,0]
    actor['animator'] = dict(clip=clip, time=phase, rate=1, loop=True, playing=file=='Fox.gltf')

# Independent poses share source bind buffers. The original textured material
# stays intact, with a slower pose on every other fox for easy visual comparison.
for row in range(3):
    for col in range(6):
        x,z = (col-2.5)*4.8, -7-row*4.8
        clip = 'Walk' if (row+col)%2 else 'Run'
        actor = add(f'Palette grove / {clip} {row*6+col+1}', 'assets/fox/Fox.gltf',
            [x,.05,z], [.022]*3)
        actor['mesh']['importMaterials'] = True
        actor['transform']['rotation'] = [0,40+col*14,0]
        actor['animator'] = dict(clip=clip,time=round(col*.13+row*.2,3),rate=.65+row*.2,loop=True,playing=True)

sculpture = add('Lua / lab status', 'torus', [0,5,-5], [2,2,2], color=(.85,.67,.25), metal=.9, rough=.18)
sculpture['script'] = 'assets/scripts/lab.lua'
scene['nextId'] = len(scene['entities'])+1
(OUT / 'project.hj.json').write_text(json.dumps(scene, separators=(',', ':'))+'\n')
print(f'Generated Animation Lab: {len(scene["entities"])} entities, original Fox + rigid-tag variant')
