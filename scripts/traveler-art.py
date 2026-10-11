#!/usr/bin/env python3
"""Original articulated traveler and transform fixture, core glTF 2.0."""
import json, math, struct
from pathlib import Path

def write(root,fixture=False):
    root.mkdir(parents=True,exist_ok=True)
    binary=bytearray();views=[];accessors=[]
    def accessor(values,width,kind='f'):
        while len(binary)%4: binary.append(0)
        offset=len(binary);binary.extend(struct.pack('<'+kind*len(values),*values))
        views.append({'buffer':0,'byteOffset':offset,'byteLength':len(binary)-offset})
        entry={'bufferView':len(views)-1,'componentType':5126 if kind=='f' else 5123,'count':len(values)//width,'type':{1:'SCALAR',2:'VEC2',3:'VEC3',4:'VEC4',16:'MAT4'}[width]}
        if width==3 and kind=='f': entry.update(min=[min(values[i::3]) for i in range(3)],max=[max(values[i::3]) for i in range(3)])
        accessors.append(entry);return len(accessors)-1
    if fixture:
        nodes=[{'translation':[0,1,0],'children':[1]},{'children':[2]},{'translation':[0,1,0]},{'mesh':0,'skin':0,'translation':[5,0,0]}]
        joints=[0,2];origins=[(0,1,0),(0,2,0)]
        positions=[-1,0,0,1,0,0,0,1,0];normals=[0,0,1]*3;uvs=[0,0,1,0,.5,1]
        joint_values=[0,1,0,0]*3;weights=[.5,.5,0,0]*3;indices=[0,1,2]
    else:
        specs=[(-1,(0,0,0)),(0,(0,-.35,0)),(1,(0,.35,0)),(2,(0,.05,0)),(3,(0,.6,0)),(3,(-.36,.34,0)),(3,(.36,.34,0)),(1,(-.17,-.08,0)),(1,(.17,-.08,0)),(7,(0,-.29,0)),(8,(0,-.29,0))]
        nodes=[];absolute=[]
        for parent,offset in specs:
            absolute.append(tuple(offset[i]+(absolute[parent][i] if parent>=0 else 0) for i in range(3)))
            nodes.append({'translation':list(offset)})
            if parent>=0:nodes[parent].setdefault('children',[]).append(len(nodes)-1)
        nodes.append({'mesh':0,'skin':0})
        joints=[1,3,4,5,6,7,8,9,10];origins=[absolute[j] for j in joints]
        positions=[];normals=[];uvs=[];joint_values=[];weights=[];indices=[]
        def ellipsoid(center,radius,joint,blend=False):
            start=len(positions)//3
            for y in range(9):
                latitude=math.pi*y/8
                for x in range(13):
                    longitude=2*math.pi*x/12
                    unit=(math.sin(latitude)*math.cos(longitude),math.cos(latitude),math.sin(latitude)*math.sin(longitude))
                    p=[center[i]+unit[i]*radius[i] for i in range(3)]
                    positions.extend(p)
                    normal=[unit[i]/radius[i] for i in range(3)];length=math.sqrt(sum(n*n for n in normal));normals.extend(n/length for n in normal)
                    uvs.extend((x/12,y/8))
                    if blend:
                        amount=max(0,min(1,(p[1]+.3)/.55));joint_values.extend((0,1,0,0));weights.extend((1-amount,amount,0,0))
                    else:joint_values.extend((joint,0,0,0));weights.extend((1,0,0,0))
            for y in range(8):
                for x in range(12):
                    a=start+y*13+x;b=a+13
                    # Exterior winding.
                    indices.extend((a,a+1,b,a+1,b+1,b))
        ellipsoid((0,-.27,0),(.29,.21,.22),0)
        ellipsoid((0,.2,0),(.29,.39,.21),1,True)
        ellipsoid((0,.75,0),(.24,.23,.24),2)
        # Front visor lies toward -Z.
        ellipsoid((0,.78,-.19),(.2,.095,.08),2)
        for side,shoulder,thigh,shin in [(-1,3,5,7),(1,4,6,8)]:
            ellipsoid((side*.37,.21,0),(.105,.29,.105),shoulder)
            ellipsoid((side*.17,-.56,0),(.13,.19,.14),thigh)
            ellipsoid((side*.17,-.79,0),(.105,.16,.105),shin)
            ellipsoid((side*.17,-.92,-.07),(.13,.07,.2),shin)
    attrs={'POSITION':accessor(positions,3),'NORMAL':accessor(normals,3),'TEXCOORD_0':accessor(uvs,2),'JOINTS_0':accessor(joint_values,4,'H'),'WEIGHTS_0':accessor(weights,4)}
    index=accessor(indices,1,'H')
    binds=[]
    for x,y,z in origins:binds.extend([1,0,0,0,0,1,0,0,0,0,1,0,-x,-y,-z,1])
    inverse=accessor(binds,16);animations=[]
    def clip(name,tracks):
        samplers=[];channels=[]
        for node,path,times,values,mode in tracks:
            samplers.append({'input':accessor(times,1),'output':accessor(values,4 if path=='rotation' else 3),'interpolation':mode})
            channels.append({'sampler':len(samplers)-1,'target':{'node':node,'path':path}})
        animations.append({'name':name,'samplers':samplers,'channels':channels})
    if fixture:
        clip('move',[(1,'translation',[0,1],[0,0,0,2,0,0],'LINEAR')])
        clip('step',[(1,'translation',[0,.5,1],[0,0,0,2,0,0,4,0,0],'STEP')])
        clip('single',[(1,'translation',[0],[2,0,0],'LINEAR')])
        clip('turn',[(1,'rotation',[0,1],[0,0,0,1,0,0,1,0],'LINEAR')])
        clip('shortest',[(1,'rotation',[0,1],[0,0,0,1,0,0,-math.sqrt(.5),-math.sqrt(.5)],'LINEAR')])
    else:
        times=[0,.25,.5,.75,1]
        def rotations(amplitude,phase):
            values=[]
            for t in times:
                angle=math.radians(amplitude)*math.sin(t*2*math.pi+phase);values.extend((math.sin(angle/2),0,0,math.cos(angle/2)))
            return values
        clip('walk',[(j,'rotation',times,rotations(a,p),'LINEAR') for j,a,p in [(5,30,0),(6,30,math.pi),(7,36,math.pi),(8,36,0),(9,18,0),(10,18,math.pi)]])
        clip('idle',[(1,'translation',[0,1.5,3],[0,-.35,0,0,-.32,0,0,-.35,0],'LINEAR')])
        clip('jump',[(j,'rotation',[0], [math.sin(math.radians(a)/2),0,0,math.cos(math.radians(a)/2)],'LINEAR') for j,a in [(5,-65),(6,-65),(7,25),(8,25),(9,-35),(10,-35)]])
    name='fixture' if fixture else 'traveler'
    doc={'asset':{'version':'2.0','generator':'Unholy original traveler art'},'buffers':[{'uri':name+'.bin','byteLength':len(binary)}],'bufferViews':views,'accessors':accessors,'meshes':[{'primitives':[{'attributes':attrs,'indices':index,'material':0}]}],'materials':[{'pbrMetallicRoughness':{'baseColorFactor':[.95,.62,.2,1],'metallicFactor':.6,'roughnessFactor':.28}}],'nodes':nodes,'skins':[{'joints':joints,'inverseBindMatrices':inverse}],'animations':animations,'scenes':[{'nodes':[0,len(nodes)-1]}],'scene':0}
    (root/(name+'.bin')).write_bytes(binary);(root/(name+'.gltf')).write_text(json.dumps(doc,separators=(',',':'))+'\n')
if __name__=='__main__':
    write(Path('projects/threshold/assets'))
    write(Path('test/fixtures/animation'),True)
