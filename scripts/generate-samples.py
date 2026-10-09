#!/usr/bin/env python3
"""Generate the checked-in sample scene data. Gameplay is Lua; the engine is Haskell."""
import json
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
ENV = dict(sunDirection=[-0.5,-1,-0.4],sunColor=[1,0.92,0.82],sunPower=3,ambient=0.14,exposure=1)
def entity(name,geometry="cube",pos=(0,0,0),scale=(1,1,1),color=(0.7,0.7,0.7),metal=0,rough=0.4,rot=(0,0,0),script=None,body=None,emission=0):
    e=dict(name=name,transform=dict(position=pos,rotation=rot,scale=scale))
    if geometry:
        e.update(mesh=dict(geometry=geometry,texture=""),material=dict(albedo=color,metallic=metal,roughness=rough,emission=emission))
    if script: e["script"]=script
    if body: e["body"]=body
    return e
def body(shape="box",mass=0,friction=0.55,bounce=0.1):
    return dict(shape=shape,mass=mass,velocity=[0,0,0],restitution=bounce,friction=friction)
def write(folder,name,entities,eye,target,env=None):
    for i,e in enumerate(entities,1):e["id"]=i
    p=dict(schemaVersion=1,name=name,nextId=len(entities)+1,entities=entities,camera=dict(eye=eye,target=target,fov=45),environment=env or ENV)
    path=ROOT/"projects"/folder/"project.hj.json"
    path.parent.mkdir(parents=True,exist_ok=True)
    path.write_text(json.dumps(p,indent=2)+"\n")

lab=[entity("Turntable","cylinder",(0,-0.2,0),(15,0.4,11),(0.21,0.24,0.29),0.3,0.6)]
for row in range(2):
    for col in range(7):
        lab.append(entity(f'{"Metal" if row==0 else "Dielectric"} / {col+1}',"sphere",(-5.4+col*1.8,0.9,-1.8+row*3.6),(1.6,1.6,1.6),(0.94,0.63,0.24) if row==0 else (0.21,0.62,0.94),1 if row==0 else 0,0.06+col*0.15))
lab.append(entity("Animated ring","torus",(0,1.55,0),(1.3,1.3,1.3),(0.75,0.78,0.85),1,0.16,script="assets/scripts/spin.lua"))
lab.append(entity("Warm point light",None,(-3,5,1)))
lab[-1]["light"]=dict(color=[1,0.7,0.42],intensity=65)
lab.append(entity("Cool point light",None,(4,4,-2)))
lab[-1]["light"]=dict(color=[0.4,0.65,1],intensity=55)
write("material-lab","Material Lab",lab,[11,9,14],[0,0.8,0])

course=[entity("Marble","sphere",(0,1,8),(1,1,1),(0.94,0.73,0.28),0.85,0.18,script="assets/scripts/marble.lua",body=body("sphere",1,0.4))]
platforms=[("Start island",(0,-0.5,8),(7,1,6)),("Blue bridge / approach",(0,-0.5,3),(2.8,1,4)),("Blue bridge / landing",(0,-0.5,-1.75),(2.8,1,3.5)),("Checkpoint island",(0,-0.5,-5),(6,1,4)),("Turn right",(5,-0.5,-5),(4,1,2.6)),("Finish island",(9,-0.5,-5),(4,1,6))]
for name,pos,size in platforms:
    color=(0.17,0.42,0.7) if "bridge" in name or name=="Turn right" else (0.21,0.53,0.41) if "Checkpoint" in name else (0.63,0.45,0.18) if "Finish" in name else (0.28,0.32,0.39)
    course.append(entity(name,"cube",pos,size,color,0.2,0.45,body=body()))
course += [entity("Checkpoint beacon","torus",(0,0.18,-5),(1.6,1.6,1.6),(0.22,0.68,0.42),0.1,0.4,rot=(90,0,0),emission=0.5),entity("Finish ring","torus",(9,1.4,-5),(2.2,2.2,2.2),(1,0.7,0.18),0.7,0.25,emission=0.5)]
for x in [-3.6,3.6]:course.append(entity("Start rail","cube",(x,0.2,8),(0.2,0.5,6),(0.34,0.38,0.45),0.5,0.3,body=body()))
course.append(entity("Finish spotlight",None,(9,5,-5)))
course[-1]["light"]=dict(color=[1,0.8,0.3],intensity=70)
write("marble-run","Marble Run",course,[10,16,22],[0,0,6])

arena=[entity("Steel ball","sphere",(0,0.7,7),(1.4,1.4,1.4),(0.6,0.7,0.8),1,0.14,script="assets/scripts/knockdown.lua",body=body("sphere",5,0.45)),entity("Arena floor","cube",(0,-0.5,0),(19,1,22),(0.23,0.26,0.32),0.2,0.5,body=body())]
for i,(x,z) in enumerate([(-5,-4),(-2,-4),(1,-4),(4,-4),(-4,-7),(-1,-7),(2,-7),(5,-7)],1):
    arena.append(entity(f"Target {i}","cube",(x,1.1,z),(0.9,2.2,0.9),(0.75,0.34,0.2),0.2,0.4,body=body("box",0.7,0.5)))
for x in [-9.6,9.6]:arena.append(entity("Side rail","cube",(x,0.3,0),(0.3,0.7,22),(0.36,0.4,0.47),0.4,0.3,body=body()))
for z in [-11.2,11.2]:arena.append(entity("End rail","cube",(0,0.3,z),(19,0.7,0.3),(0.36,0.4,0.47),0.4,0.3,body=body()))
write("knockdown","Knockdown",arena,[17,19,24],[0,0,-1])
print("Generated Material Lab, Marble Run and Knockdown.")
