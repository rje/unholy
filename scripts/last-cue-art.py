#!/usr/bin/env python3
"""Original theatre kit, articulated stagehand and PCM sounds. No raster edits."""
import importlib.util
import json
import math
import shutil
import struct
import wave
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
ART = ROOT/'projects/last-cue/assets'
ART.mkdir(parents=True, exist_ok=True)


def box(center, size):
    x, y, z = center
    a, b, c = (v/2 for v in size)
    corners = [(x+sx*a, y+sy*b, z+sz*c) for sx, sy, sz in
               [(-1,-1,-1),(1,-1,-1),(1,1,-1),(-1,1,-1),(-1,-1,1),(1,-1,1),(1,1,1),(-1,1,1)]]
    return [tuple(corners[i] for i in face) for face in
            [(0,3,2),(0,2,1),(4,5,6),(4,6,7),(0,4,7),(0,7,3),(1,2,6),(1,6,5),(3,7,6),(3,6,2),(0,1,5),(0,5,4)]]


def ring(center, radius, thickness, depth):
    result = []
    x, y, z = center
    for i in range(24):
        angles = [math.tau*i/24, math.tau*(i+1)/24]
        p = [(x+r*math.cos(a), y+r*math.sin(a), z+d) for d in [-depth/2,depth/2]
             for r in [radius-thickness,radius] for a in angles]
        for face in [(0,2,3),(0,3,1),(4,5,7),(4,7,6),(2,6,7),(2,7,3),(0,1,5),(0,5,4)]:
            result.append(tuple(p[j] for j in reversed(face)))
    return result


def write_mesh(name, faces, repeat=1):
    binary = bytearray()
    views, accessors = [], []
    def accessor(values, width, kind):
        offset = len(binary)
        binary.extend(struct.pack('<'+'f'*len(values), *values))
        views.append(dict(buffer=0, byteOffset=offset, byteLength=len(binary)-offset))
        entry = dict(bufferView=len(views)-1, componentType=5126, count=len(values)//width, type=kind)
        if kind == 'VEC3':
            entry.update(min=[min(values[i::3]) for i in range(3)],max=[max(values[i::3]) for i in range(3)])
        accessors.append(entry)
        return len(accessors)-1
    positions, normals, uv = [], [], []
    for number, face in enumerate(faces):
        p, q, r = face
        u, v = [q[i]-p[i] for i in range(3)], [r[i]-p[i] for i in range(3)]
        n = [u[1]*v[2]-u[2]*v[1],u[2]*v[0]-u[0]*v[2],u[0]*v[1]-u[1]*v[0]]
        length = math.sqrt(sum(t*t for t in n))
        if length < 1e-8:
            continue
        for point, tex in zip(face, [(0,0),(repeat,0),(repeat,repeat)] if number%2==0 else [(0,0),(repeat,repeat),(0,repeat)]):
            positions.extend(point)
            normals.extend(t/length for t in n)
            uv.extend(tex)
    attributes = dict(POSITION=accessor(positions,3,'VEC3'),NORMAL=accessor(normals,3,'VEC3'),TEXCOORD_0=accessor(uv,2,'VEC2'))
    doc = dict(asset={'version':'2.0','generator':'Unholy original theatre kit'},buffers=[dict(uri=name+'.bin',byteLength=len(binary))],bufferViews=views,accessors=accessors,
               meshes=[dict(primitives=[dict(attributes=attributes)])],nodes=[dict(mesh=0)],scenes=[dict(nodes=[0])],scene=0)
    (ART/(name+'.bin')).write_bytes(binary)
    (ART/(name+'.gltf')).write_text(json.dumps(doc,separators=(',',':'))+'\n')


write_mesh('deck', box((0,0,0),(1,1,1)), 6)
write_mesh('key', ring((0,0,0),.30,.09,.13)+box((0,-.48,0),(.12,.55,.12))+box((.12,-.65,0),(.30,.12,.12)))
wheel = ring((0,0,0),.64,.11,.18)+box((0,0,0),(.18,1.05,.15))+box((0,0,0),(1.05,.18,.15))
write_mesh('pulley', wheel)
write_mesh('winch', box((0,.38,0),(1.65,.75,1.1))+box((-.64,1.08,0),(.18,1.1,.3))+box((.64,1.08,0),(.18,1.1,.3))
           +ring((0,1.42,0),.62,.10,.38)+box((0,1.42,0),(.95,.10,.35))+box((0,1.42,0),(.12,.95,.35)))
write_mesh('flat', box((0,2,0),(2.5,4,.20))+box((0,.15,.65),(2.5,.3,1.4))+box((0,2.7,.5),(.14,.14,1.2)))
curtain = []
for i in range(32):
    a, b = i/32*2-1, (i+1)/32*2-1
    def p(x,y): return (x,y,.18*math.cos(x*math.pi*6))
    for lower, upper in [(0,3.5),(3.5,7)]:
        curtain.extend([(p(a,lower),p(b,lower),p(b,upper)),(p(a,lower),p(b,upper),p(a,upper)),
                        (p(a,lower),p(b,upper),p(b,lower)),(p(a,lower),p(a,upper),p(b,upper))])
write_mesh('curtain', curtain)
arch = box((-7.3,3.5,0),(1,7,.8))+box((7.3,3.5,0),(1,7,.8))+box((0,7.2,0),(15.6,.5,1))
for i in range(12):
    angle = math.pi*(i+.5)/12
    arch += box((7*math.cos(angle),5.3+1.4*math.sin(angle),0),(.9,.28,1.1))
write_mesh('arch', arch)
weights = box((0,2.4,0),(.7,5,.5))
for height in [.3,.65,1,1.35,1.7,2.05]:
    weights += box((0,height,0),(1.1,.25,.8))
weights += ring((0,5.1,0),.32,.08,.18)
write_mesh('counterweight', weights)

# Reuse our original articulated rig generator, retaining skinning/attachments.
spec = importlib.util.spec_from_file_location('traveler',ROOT/'scripts/traveler-art.py')
traveler = importlib.util.module_from_spec(spec)
spec.loader.exec_module(traveler)
traveler.write(ART)
rig = json.loads((ART/'traveler.gltf').read_text())
rig['asset']['generator'] = 'Unholy original articulated theatre stagehand'
rig['buffers'][0]['uri'] = 'stagehand.bin'
shutil.move(ART/'traveler.bin',ART/'stagehand.bin')
primitive = rig['meshes'][0]['primitives'][0]
indices = rig['accessors'][primitive['indices']]
rig['materials'] = [
    {'pbrMetallicRoughness':{'baseColorFactor':[.09,.18,.24,1],'metallicFactor':.05,'roughnessFactor':.8}},
    {'pbrMetallicRoughness':{'baseColorFactor':[.76,.51,.27,1],'metallicFactor':.1,'roughnessFactor':.62}},
    {'pbrMetallicRoughness':{'baseColorFactor':[.04,.045,.05,1],'metallicFactor':.2,'roughnessFactor':.48}},
]
parts = []
for i, material in enumerate([0,0,1,2,0,0,1,2,0,0,1,2]):
    entry = dict(indices, count=576, byteOffset=i*576*2)
    rig['accessors'].append(entry)
    parts.append(dict(primitive, indices=len(rig['accessors'])-1, material=material))
rig['meshes'][0]['primitives'] = parts
(ART/'stagehand.gltf').write_text(json.dumps(rig,separators=(',',':'))+'\n')
(ART/'traveler.gltf').unlink()
shutil.copyfile(ROOT/'projects/material-gallery/assets/studio.hdr',ART/'studio.hdr')

(ART/'audio').mkdir(exist_ok=True)
rate = 22050
def sound(name, duration, signal):
    with wave.open(str(ART/'audio'/(name+'.wav')),'wb') as f:
        f.setnchannels(1);f.setsampwidth(2);f.setframerate(rate)
        f.writeframes(b''.join(struct.pack('<h',round(max(-1,min(1,signal(i/rate)))*32767)) for i in range(round(duration*rate))))
def tone(t,f,d): return math.sin(math.tau*f*t)*math.exp(-t*d)
sound('step',.16,lambda t:(tone(t,87,40)+.2*tone(t,390,60))*.48)
sound('key',.7,lambda t:(tone(t,880,8)+.4*tone(t,1320,6))*.4)
sound('whistle',.65,lambda t:math.sin(math.tau*(1450*t+70*t*t))*math.sin(math.pi*t/.65)*.4)
sound('winch',2,lambda t:(tone(t,83,1.6)+.3*math.sin(math.tau*167*t))*math.sin(math.pi*t/2)*.32)
sound('curtain',3,lambda t:sum(tone(t-d,f,2.2) for d,f in [(0,261.63),(.15,329.63),(.3,392),(.8,523.25)] if t>=d)*.28)
sound('room',4,lambda t:(math.sin(math.tau*55*t)+.12*math.sin(math.tau*165*t))*.05*math.sin(math.pi*t/4)**2)
print('Generated original theatre meshes, stagehand and six WAV clips')
