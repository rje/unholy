#!/usr/bin/env python3
"""Generate our own small, reproducible static glTF PBR qualification assets."""
import json, math, struct, copy
from pathlib import Path
from PIL import Image

ROOT=Path(__file__).resolve().parents[1]
OUT=ROOT/'projects/material-gallery'
ART=OUT/'assets'
ART.mkdir(parents=True,exist_ok=True)
size=64
base=Image.new('RGB',(size,size)); packed=Image.new('RGB',(size,size)); normal=Image.new('RGB',(size,size))
for y in range(size):
    for x in range(size):
        check=(x//8+y//8)%2
        base.putpixel((x,y),(170,115,55) if check else (220,195,140))
        packed.putpixel((x,y),(255,48 if check else 200,210 if check else 20))
        angle=math.sin(x*math.pi/8)*0.65
        normal.putpixel((x,y),(int(127.5+100*angle),128,int(127.5+127.5*math.sqrt(1-angle*angle))))
base.save(ART/'bronze.png');packed.save(ART/'metal-rough.png');normal.save(ART/'ridges.png')
Image.new('RGB',(2,2),(192,192,192)).save(ART/'gray.png')
Image.new('RGB',(2,2),(220,128,236)).save(ART/'directional-normal.png')
materials=[
    {'name':'Factor-only ruby','pbrMetallicRoughness':{'baseColorFactor':[0.65,0.035,0.06,1],'metallicFactor':0.1,'roughnessFactor':0.24}},
    {'name':'Factor-only cobalt','pbrMetallicRoughness':{'baseColorFactor':[0.025,0.12,0.72,1],'metallicFactor':0.85,'roughnessFactor':0.13}},
    {'name':'Patterned bronze','pbrMetallicRoughness':{'baseColorFactor':[0.8,0.65,0.45,1],'baseColorTexture':{'index':0},'metallicFactor':0.85,'roughnessFactor':0.8,'metallicRoughnessTexture':{'index':1}}},
    {'name':'Ridged silver','pbrMetallicRoughness':{'baseColorFactor':[0.65,0.7,0.75,1],'metallicFactor':0.95,'roughnessFactor':0.3},'normalTexture':{'index':2,'scale':1.4}},
    {'name':'Jade emission','pbrMetallicRoughness':{'baseColorFactor':[0.025,0.35,0.16,1],'metallicFactor':0.25,'roughnessFactor':0.45},'emissiveFactor':[0.015,0.12,0.035]},
    {'name':'Linear factor + sRGB gray','pbrMetallicRoughness':{'baseColorFactor':[0.3,0.12,0.65,1],'baseColorTexture':{'index':3},'metallicFactor':0,'roughnessFactor':0.7}},
]

def write_model(name, surfaces, mats=materials):
    binary=bytearray();views=[];accessors=[];primitives=[]
    def accessor(values,fmt,kind,count,components):
        while len(binary)%4:binary.append(0)
        offset=len(binary)
        binary.extend(struct.pack('<'+fmt*len(values),*values))
        views.append({'buffer':0,'byteOffset':offset,'byteLength':len(binary)-offset})
        item={'bufferView':len(views)-1,'componentType':5126 if fmt=='f' else 5123,'count':count,'type':kind}
        if kind=='VEC3':
            item['min']=[min(values[i::components]) for i in range(components)]
            item['max']=[max(values[i::components]) for i in range(components)]
        accessors.append(item);return len(accessors)-1
    for positions,normals,uv,indices,material in surfaces:
        count=len(positions)//3
        attributes={'POSITION':accessor(positions,'f','VEC3',count,3),'NORMAL':accessor(normals,'f','VEC3',count,3),'TEXCOORD_0':accessor(uv,'f','VEC2',count,2)}
        primitives.append({'attributes':attributes,'indices':accessor(indices,'H','SCALAR',len(indices),1),'material':material})
    model={'asset':{'version':'2.0','generator':'Unholy authored material gallery'},'scene':0,'scenes':[{'nodes':[0]}],'nodes':[{'mesh':0}],
        'meshes':[{'primitives':primitives}],'materials':mats,'buffers':[{'uri':name+'.bin','byteLength':len(binary)}],'bufferViews':views,'accessors':accessors,
        'images':[{'uri':p} for p in ['bronze.png','metal-rough.png','ridges.png','gray.png','directional-normal.png']],
        'textures':[{'source':i} for i in range(5)]}
    (ART/(name+'.bin')).write_bytes(binary)
    (ART/(name+'.gltf')).write_text(json.dumps(model,indent=2)+'\n')
    payload=json.dumps(model,separators=(',',':')).encode();payload+=b' '*((-len(payload))%4)
    # GLB metadata coverage; retain external buffer/images deliberately.
    (ART/(name+'.glb')).write_bytes(struct.pack('<III',0x46546c67,2,20+len(payload))+struct.pack('<II',len(payload),0x4e4f534a)+payload)

surfaces=[]
for material in range(6):
    cx=(material%3-1)*3.2;cz=(-1 if material<3 else 1)*2.6
    positions=[];normals=[];uv=[];indices=[];rings=32;segments=48
    for j in range(rings+1):
        v=j/rings;y=v*3
        radius=0.58+0.22*math.sin(v*math.pi*3)+0.11*math.cos(v*math.pi*6)
        slope=(0.22*math.pi*3*math.cos(v*math.pi*3)-0.11*math.pi*6*math.sin(v*math.pi*6))/3
        for i in range(segments+1):
            theta=i/segments*2*math.pi; c=math.cos(theta);s=math.sin(theta);length=math.sqrt(1+slope*slope)
            positions.extend([cx+radius*c,0.6+y,cz+radius*s]);normals.extend([c/length,-slope/length,s/length]);uv.extend([i/segments,v])
    for j in range(rings):
        for i in range(segments):
            a=j*(segments+1)+i;b=a+segments+1;indices.extend([a,b,a+1,a+1,b,b+1])
    surfaces.append((positions,normals,uv,indices,material))
write_model('reliquaries',surfaces)

cards=[]
for i in range(6):
    x=(i-2.5)*2.4
    cards.append(([x-1,-1,0,x+1,-1,0,x+1,1,0,x-1,1,0],[0,0,1]*4,[0,1,1,1,1,0,0,0],[0,1,2,0,2,3],i))
card_materials=copy.deepcopy(materials)
card_materials[3]['normalTexture']={'index':4,'scale':1.4}
card_materials[3]['pbrMetallicRoughness'].update(metallicFactor=0,roughnessFactor=0.55)
write_model('cards',cards,card_materials)
positions,normals,uv,indices,material=cards[3]
write_model('normal-mirror',[(positions,normals,[1-u if i%2==0 else u for i,u in enumerate(uv)],indices,material)],card_materials)
write_model('normal-degenerate',[(positions,normals,[0]*len(uv),indices,material)],card_materials)

baseline=json.loads((ROOT/'projects/circuit-breaker/project.hj.json').read_text())
baseline.update(name='Material Gallery',haskellGame=None,nextId=11)
def entity(id,name,geometry,position,scale,color,metal=0,rough=0.4,imported=False):
    return {'id':id,'name':name,'transform':{'position':position,'rotation':[0,0,0],'scale':scale},
        'mesh':{'geometry':geometry,'texture':'','importMaterials':imported},'material':{'albedo':color,'metallic':metal,'roughness':rough,'emission':0}}
baseline['entities']=[entity(1,'Six imported material slots','assets/reliquaries.gltf',[0,0,0],[1,1,1],[0.72,0.78,0.85],imported=True),entity(2,'Gallery floor','cube',[0,-0.3,0],[15,0.6,13],[0.07,0.09,0.13],0.3,0.4)]
for i in range(6):
    baseline['entities'].append(entity(i+3,materials[i]['name'],'cylinder',[(i%3-1)*3.2,0.3,(-1 if i<3 else 1)*2.6],[2,0.6,2],[0.22,0.25,0.29],0.6,0.23))
for id,pos,color,power in [(9,[4,6,6],[0.65,0.8,1],120),(10,[-4,5,0],[1,0.8,0.6],100)]:
    baseline['entities'].append({'id':id,'name':'Gallery fill','transform':{'position':pos,'rotation':[0,0,0],'scale':[1,1,1]},'light':{'color':color,'intensity':power}})
baseline['camera']={'eye':[13,10,16],'target':[0,1,0],'fov':45}
baseline['environment'].update(sunDirection=[-0.5,-1,-0.4],sunPower=3,ambient=0.27,fogDensity=0.008,sunShadows=True)
(OUT/'project.hj.json').write_text(json.dumps(baseline,indent=2)+'\n')
print(OUT)
