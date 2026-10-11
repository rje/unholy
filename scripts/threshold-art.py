#!/usr/bin/env python3
"""Original static collision/render causeway and reused original sample art."""
import json
import math
import shutil
import struct
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
ART = ROOT/'projects/threshold/assets'
ART.mkdir(parents=True, exist_ok=True)
binary = bytearray()
views, accessors = [], []


def accessor(values, components, kind):
    offset = len(binary)
    binary.extend(struct.pack('<'+'f'*len(values), *values))
    views.append(dict(buffer=0, byteOffset=offset, byteLength=len(binary)-offset))
    accessors.append(dict(bufferView=len(views)-1, componentType=5126, count=len(values)//components, type=kind))
    return len(accessors)-1


a, b, c, d = (-3, 0, 6), (3, 0, 6), (-3, 2, 0), (3, 2, 0)
e, f = (-3, 0, 0), (3, 0, 0)
faces = [(a, c, d), (a, d, b), (a, e, c), (b, d, f), (e, f, d), (e, d, c), (a, b, f), (a, f, e)]
positions, normals, uv = [], [], []
for face in faces:
    p, q, r = face
    u, v = [q[i]-p[i] for i in range(3)], [r[i]-p[i] for i in range(3)]
    n = (u[1]*v[2]-u[2]*v[1], u[2]*v[0]-u[0]*v[2], u[0]*v[1]-u[1]*v[0])
    length = math.sqrt(sum(x*x for x in n))
    for point in face:
        positions.extend(point)
        normals.extend(x/length for x in n)
        uv.extend((point[0]/2, point[2]/2))
attributes = dict(POSITION=accessor(positions, 3, 'VEC3'), NORMAL=accessor(normals, 3, 'VEC3'), TEXCOORD_0=accessor(uv, 2, 'VEC2'))
model = dict(asset={'version': '2.0'}, buffers=[dict(uri='ramp.bin', byteLength=len(binary))], bufferViews=views, accessors=accessors,
    meshes=[dict(primitives=[dict(attributes=attributes)])], nodes=[dict(mesh=0)], scenes=[dict(nodes=[0])], scene=0)
(ART/'ramp.bin').write_bytes(binary)
(ART/'ramp.gltf').write_text(json.dumps(model, indent=2)+'\n')
shutil.copyfile(ROOT/'projects/material-gallery/assets/studio.hdr', ART/'studio.hdr')
(ART/'audio').mkdir(exist_ok=True)
for source, name in [('jump.wav', 'jump.wav'), ('checkpoint.wav', 'pickup.wav')]:
    shutil.copyfile(ROOT/'projects/marble-run/assets/audio'/source, ART/'audio'/name)
