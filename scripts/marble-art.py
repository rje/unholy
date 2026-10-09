"""Run with Blender --background --python scripts/marble-art.py.
Produces small static OBJ assets with UVs and bevels; no runtime Blender dependency.
"""
from pathlib import Path
import bpy
import bmesh
import math
import random
from mathutils import Vector

ROOT=Path(__file__).resolve().parents[1]
OUT=ROOT/'projects/marble-run/assets/models'
OUT.mkdir(parents=True,exist_ok=True)
random.seed(41)

def clear():
    bpy.ops.object.select_all(action='SELECT')
    bpy.ops.object.delete(use_global=False)

def export(name):
    bpy.ops.object.select_all(action='SELECT')
    for obj in bpy.context.selected_objects:
        bpy.context.view_layer.objects.active=obj
        if obj.type=='MESH':
            bm=bmesh.new();bm.from_mesh(obj.data)
            bmesh.ops.recalc_face_normals(bm,faces=list(bm.faces));bm.to_mesh(obj.data);bm.free()
        for mod in list(obj.modifiers): bpy.ops.object.modifier_apply(modifier=mod.name)
        if obj.type!='MESH': bpy.ops.object.convert(target='MESH')
        mesh=obj.data
        if not mesh.uv_layers: mesh.uv_layers.new(name='UVMap')
        for face in mesh.polygons:
            normal=face.normal
            axis=max(range(3),key=lambda i:abs(normal[i]))
            axes=[i for i in range(3) if i!=axis]
            for li in face.loop_indices:
                co=mesh.vertices[mesh.loops[li].vertex_index].co
                mesh.uv_layers.active.data[li].uv=(co[axes[0]]/5.5,co[axes[1]]/5.5)
    bpy.ops.wm.obj_export(filepath=str(OUT/(name+'.obj')),export_materials=False,
        export_selected_objects=True,forward_axis='NEGATIVE_Z',up_axis='Y',export_triangulated_mesh=True)

def bevel(obj,width):
    mod=obj.modifiers.new('Soft machined edges','BEVEL');mod.width=width;mod.segments=3
    mod=obj.modifiers.new('Face normals','WEIGHTED_NORMAL');mod.keep_sharp=True;mod.weight=50

def slab(name,size,width=.16):
    clear()
    # Blender Z-up to engine Y-up; normalize geometry to a unit box after beveling.
    bpy.ops.mesh.primitive_cube_add(size=1)
    obj=bpy.context.object;obj.dimensions=(size[0],size[2],size[1])
    bpy.ops.object.transform_apply(location=False,rotation=False,scale=True)
    bevel(obj,width)
    export(name)
    # Preserve world-sized UVs while giving the ECS/collider the familiar unit extent.
    p=OUT/(name+'.obj');lines=[]
    for line in p.read_text().splitlines():
        if line.startswith('v '):
            v=list(map(float,line.split()[1:]));line='v '+' '.join(str(v[i]/size[i]) for i in range(3))
        elif line.startswith('vn '):
            v=[float(n)*size[i] for i,n in enumerate(line.split()[1:])]
            length=math.sqrt(sum(n*n for n in v));line='vn '+' '.join(str(n/length) for n in v)
        lines.append(line)
    p.write_text('\n'.join(lines)+'\n')

for name,size in [('start',(7,1,6)),('approach',(3.2,1,4)),('landing',(3.2,1,3.5)),
    ('checkpoint',(6,1,4)),('shuttle',(4.4,1,4)),('finish',(4,1,6)),('lift',(3,1,3)),('slab',(1,1,1))]:
    slab(name,size,.14 if name!='slab' else .08)

clear()
vertices=[];rings=[(.48,.35),(.58,.0),(.45,-.35),(.18,-.85)]
for ri,(radius,z) in enumerate(rings):
    for i in range(16):
        a=i*math.tau/16;r=radius*random.uniform(.84,1.15)
        vertices.append((r*math.cos(a)+ri*.018,r*math.sin(a),z+random.uniform(-.035,.035)))
faces=[tuple(range(15,-1,-1))]
for r in range(3):
    for i in range(16):
        a=r*16+i;b=r*16+(i+1)%16;c=b+16;d=a+16
        faces.extend([(a,b,d),(b,c,d)])
faces.append(tuple(range(48,64)))
mesh=bpy.data.meshes.new('Faceted hanging limestone');mesh.from_pydata(vertices,[],faces);mesh.update()
obj=bpy.data.objects.new('Floating island',mesh);bpy.context.collection.objects.link(obj)
export('island')

clear()
for z,r,h in [(-.44,.5,.12),(-.32,.36,.12),(0,.24,.58),(.32,.36,.12),(.44,.5,.12)]:
    bpy.ops.mesh.primitive_cylinder_add(vertices=32,radius=r,depth=h,location=(0,0,z))
    bevel(bpy.context.object,.025)
export('pillar')

clear()
# A horseshoe gateway in the engine XY plane, with a thick sculpted arch.
verts=[];faces=[]
for i in range(41):
    a=math.radians(-25+230*i/40)
    for r,depth in [(1.0,-.17),(1.0,.17),(1.23,-.17),(1.23,.17)]:
        verts.append((r*math.cos(a),depth,r*math.sin(a)))
for i in range(40):
    j=i*4;k=j+4
    faces.extend([(j,k,k+1,j+1),(j+2,j+3,k+3,k+2),(j,j+2,k+2,k),(j+1,k+1,k+3,j+3)])
faces.extend([(0,1,3,2),(160,162,163,161)])
mesh=bpy.data.meshes.new('Gateway');mesh.from_pydata(verts,[],faces);mesh.update()
obj=bpy.data.objects.new('Gateway',mesh);bpy.context.collection.objects.link(obj);bevel(obj,.04)
export('gateway')

clear()
bpy.ops.mesh.primitive_cone_add(vertices=6,radius1=.42,radius2=.13,depth=1)
obj=bpy.context.object
for v in obj.data.vertices:
    if v.co.z>0: v.co.z+=random.uniform(.15,.5)
export('crystal')

clear()
for location,scale in [((0,0,.3),(.55,.45,.4)),((-.35,0,0),(.45,.4,.35)),((.28,.1,.0),(.5,.38,.36))]:
    bpy.ops.mesh.primitive_ico_sphere_add(subdivisions=2,radius=1,location=location)
    bpy.context.object.scale=scale
    bpy.ops.object.transform_apply(location=False,rotation=False,scale=True)
export('canopy')
print('Marble Run art exported to',OUT)
