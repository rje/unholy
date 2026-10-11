"""Original Last Cue art, authored/exported by Blender in background mode.

Run through last-cue-art.py. Coordinates in the modeling helpers are Unholy's
X/right, Y/up, -Z/forward; Blender's glTF exporter handles the axis conversion.
The saved .blend files retain meters and named collections. Only the few unit
box replacements are normalized during export to preserve gameplay colliders.
"""
import json
import math
import random
import sys
from pathlib import Path

import bpy
from mathutils import Matrix, Vector

ROOT = Path(__file__).resolve().parents[1]
ASSETS = ROOT / "projects/last-cue/assets"
SOURCE = ROOT / "projects/last-cue/art"
ASSETS.mkdir(parents=True, exist_ok=True)
SOURCE.mkdir(parents=True, exist_ok=True)
random.seed(19)
bpy.ops.wm.read_factory_settings(use_empty=True)


def xyz(p):
    return Vector((p[0], -p[2], p[1]))


def active(o):
    bpy.ops.object.select_all(action="DESELECT")
    o.select_set(True)
    bpy.context.view_layer.objects.active = o


def material(name, color, metal=0, rough=.65, texture=None, normal=None):
    m = bpy.data.materials.new(name)
    m.use_nodes = True
    m.diffuse_color = (*color, 1)
    p = m.node_tree.nodes.get("Principled BSDF")
    p.inputs["Base Color"].default_value = (*color, 1)
    p.inputs["Metallic"].default_value = metal
    p.inputs["Roughness"].default_value = rough
    # Keep the export in the engine's intentionally supported core PBR subset.
    p.inputs["Specular IOR Level"].default_value = .5
    if texture:
        tex = m.node_tree.nodes.new("ShaderNodeTexImage")
        tex.image = bpy.data.images.load(str(ASSETS / texture), check_existing=True)
        if color == (1, 1, 1):
            m.node_tree.links.new(tex.outputs["Color"], p.inputs["Base Color"])
        else:
            mix = m.node_tree.nodes.new("ShaderNodeMixRGB")
            mix.blend_type = "MULTIPLY"
            mix.inputs[0].default_value = 1
            mix.inputs[2].default_value = (*color, 1)
            m.node_tree.links.new(tex.outputs["Color"], mix.inputs[1])
            m.node_tree.links.new(mix.outputs[0], p.inputs["Base Color"])
    if normal:
        tex = m.node_tree.nodes.new("ShaderNodeTexImage")
        tex.image = bpy.data.images.load(str(ASSETS / normal), check_existing=True)
        tex.image.colorspace_settings.name = "Non-Color"
        bump = m.node_tree.nodes.new("ShaderNodeNormalMap")
        bump.inputs["Strength"].default_value = .4
        m.node_tree.links.new(tex.outputs["Color"], bump.inputs["Color"])
        m.node_tree.links.new(bump.outputs["Normal"], p.inputs["Normal"])
    return m


def bake_wood():
    """Bake restrained grain and tangent normals with Blender, no raster edits."""
    scene = bpy.context.scene
    scene.render.engine = "CYCLES"
    scene.cycles.samples = 1
    scene.cycles.device = "CPU"
    bpy.ops.mesh.primitive_plane_add(size=2)
    plane = bpy.context.object
    m = material("Wood bake", (.4, .25, .12))
    plane.data.materials.append(m)
    nodes, links = m.node_tree.nodes, m.node_tree.links
    p = nodes.get("Principled BSDF")
    uv = nodes.new("ShaderNodeTexCoord")
    mapping = nodes.new("ShaderNodeVectorMath")
    mapping.operation = "MULTIPLY"
    mapping.inputs[1].default_value = (5, 100, 1)
    links.new(uv.outputs["UV"], mapping.inputs[0])
    noise = nodes.new("ShaderNodeTexNoise")
    noise.inputs["Scale"].default_value = 2
    noise.inputs["Detail"].default_value = 3
    noise.inputs["Roughness"].default_value = .7
    links.new(mapping.outputs[0], noise.inputs["Vector"])
    ramp = nodes.new("ShaderNodeValToRGB")
    ramp.color_ramp.elements[0].position = .12
    ramp.color_ramp.elements[0].color = (.20, .10, .045, 1)
    ramp.color_ramp.elements[1].position = .88
    ramp.color_ramp.elements[1].color = (.38, .23, .11, 1)
    links.new(noise.outputs["Fac"], ramp.inputs[0])
    links.new(ramp.outputs[0], p.inputs["Base Color"])
    bump = nodes.new("ShaderNodeBump")
    bump.inputs["Distance"].default_value = .003
    links.new(noise.outputs["Fac"], bump.inputs["Height"])
    links.new(bump.outputs[0], p.inputs["Normal"])
    for name, kind in [("wood.png", "DIFFUSE"), ("wood-normal.png", "NORMAL")]:
        im = bpy.data.images.new(name, width=512, height=512)
        if kind == "NORMAL":
            im.colorspace_settings.name = "Non-Color"
        target = nodes.new("ShaderNodeTexImage")
        target.image = im
        nodes.active = target
        scene.render.bake.use_pass_direct = False
        scene.render.bake.use_pass_indirect = False
        scene.render.bake.use_pass_color = True
        active(plane)
        bpy.ops.object.bake(type=kind, margin=8)
        im.filepath_raw = str(ASSETS / name)
        im.file_format = "PNG"
        im.save()
    bpy.data.objects.remove(plane, do_unlink=True)


if "--characters-only" not in sys.argv:
    bake_wood()
IRON = material("Blackened cast iron", (.055, .068, .078), .8, .38)
BRASS = material("Worn brass", (.55, .33, .095), .78, .36)
GOLD = material("Old gilding", (.66, .43, .16), .65, .43)
RUST = material("Red oxide enamel", (.23, .075, .045), .28, .62)
TEAL = material("Petrol painted timber", (.055, .13, .14), 0, .76)
IVORY = material("Aged scenic plaster", (.59, .48, .32), 0, .85)
ROPE = material("Hemp rope", (.38, .29, .16), 0, .93)
VELVET = material("Oxblood velvet", (1, 1, 1), 0, .91, "velvet.png")
WOODS = [material("Oak %d" % i, (1, 1, 1), 0, .76, "wood.png", "wood-normal.png") for i in range(5)]
# A factor on textured materials is explicit in glTF after export (see export).
BRICKS = [material("Soot brick %d" % i, (.12 + i*.012, .095 + i*.009, .084 + i*.007), 0, .9) for i in range(5)]
MORTAR = material("Dark mortar", (.055, .05, .046), 0, 1)
current = None


def finish(o, name, mat, smooth=False, bone=None):
    o.name = name
    for col in list(o.users_collection):
        col.objects.unlink(o)
    current.objects.link(o)
    if mat:
        o.data.materials.append(mat)
    if o.type == "MESH":
        for p in o.data.polygons:
            p.use_smooth = smooth
    if bone:
        group = o.vertex_groups.new(name=bone)
        group.add(list(range(len(o.data.vertices))), 1, "REPLACE")
    return o


def box(name, center, size, mat, bevel=.025, bone=None, segments=3):
    bpy.ops.mesh.primitive_cube_add(size=1, location=xyz(center))
    o = bpy.context.object
    o.scale = (size[0], size[2], size[1])
    bpy.ops.object.transform_apply(location=False, rotation=False, scale=True)
    if bevel:
        mod = o.modifiers.new("Soft manufactured edges", "BEVEL")
        mod.width = min(bevel, min(size)*.2)
        mod.segments = segments
        bpy.ops.object.modifier_apply(modifier=mod.name)
        mod = o.modifiers.new("Weighted corner normals", "WEIGHTED_NORMAL")
        mod.keep_sharp = True
        bpy.ops.object.modifier_apply(modifier=mod.name)
    # UVs follow the long axis for each plank rather than every triangle.
    if mat in WOODS:
        for loop in o.data.uv_layers.active.data:
            loop.uv = (loop.uv.x, loop.uv.y)
    return finish(o, name, mat, bone=bone)


def sphere(name, center, size, mat, bone=None):
    bpy.ops.mesh.primitive_uv_sphere_add(segments=24, ring_count=12, radius=1, location=xyz(center))
    o = bpy.context.object
    o.scale = (size[0]/2, size[2]/2, size[1]/2)
    bpy.ops.object.transform_apply(location=False, rotation=False, scale=True)
    return finish(o, name, mat, True, bone)


def rod(name, a, b, radius, mat, vertices=16, bone=None):
    delta = xyz(b) - xyz(a)
    bpy.ops.mesh.primitive_cylinder_add(vertices=vertices, radius=radius, depth=delta.length, location=(xyz(a)+xyz(b))/2)
    o = bpy.context.object
    o.rotation_mode = "QUATERNION"
    o.rotation_quaternion = delta.to_track_quat("Z", "Y")
    bpy.ops.object.transform_apply(location=False, rotation=True, scale=True)
    return finish(o, name, mat, True, bone)


def line(name, points, radius, mat, bone=None, cyclic=False):
    curve = bpy.data.curves.new(name, "CURVE")
    curve.dimensions = "3D"
    curve.resolution_u = 1
    curve.bevel_depth = radius
    curve.bevel_resolution = 2
    curve.use_fill_caps = True
    poly = curve.splines.new("POLY")
    poly.points.add(len(points)-1)
    for p, v in zip(poly.points, points):
        p.co = (*xyz(v), 1)
    poly.use_cyclic_u = cyclic
    o = bpy.data.objects.new(name, curve)
    current.objects.link(o)
    active(o)
    bpy.ops.object.convert(target="MESH")
    return finish(bpy.context.object, name, mat, True, bone)


def ring(name, center, radius, tube, mat, bone=None, axis="z"):
    x, y, z = center
    points = []
    for i in range(64):
        a = math.tau*i/64
        points.append((x+radius*math.cos(a), y+radius*math.sin(a), z) if axis == "z" else (x, y+radius*math.sin(a), z+radius*math.cos(a)))
    return line(name, points, tube, mat, bone, True)


def bolt(center, radius=.04, bone=None):
    x, y, z = center
    return rod("Hex fastening", (x,y,z-.018), (x,y,z+.018), radius, BRASS, 6, bone)


def collection(name):
    global current
    current = bpy.data.collections.new(name)
    bpy.context.scene.collection.children.link(current)
    return current


def join_meshes(col):
    meshes = [o for o in col.objects if o.type == "MESH"]
    if len(meshes) == 1:
        return meshes[0]
    bpy.ops.object.select_all(action="DESELECT")
    for o in meshes:
        o.select_set(True)
    bpy.context.view_layer.objects.active = meshes[0]
    bpy.ops.object.join()
    o = meshes[0]
    bpy.context.scene.cursor.location = (0,0,0)
    bpy.ops.object.origin_set(type="ORIGIN_CURSOR")
    o.name = col.name
    return o


def export(col, normalized=None, rig=False):
    o = join_meshes(col)
    # The .blend stays editable at real size; glTF uses unit-box transforms where
    # requested, so all existing Bullet box dimensions remain identical.
    transform = None
    if normalized:
        sx, sy, sz = normalized
        transform = Matrix.Diagonal((1/sx, 1/sz, 1/sy, 1))
        o.data.transform(transform)
    bpy.ops.object.select_all(action="DESELECT")
    for obj in col.objects:
        obj.select_set(True)
    bpy.context.view_layer.objects.active = o
    bpy.ops.export_scene.gltf(filepath=str(ASSETS / (col.name+".gltf")), export_format="GLTF_SEPARATE", use_selection=True,
        export_keep_originals=True, export_texcoords=True, export_normals=True,
        export_vertex_color="NONE", export_materials="EXPORT", export_animations=rig,
        export_animation_mode="ACTIONS", export_force_sampling=True,
        export_sampling_interpolation_fallback="LINEAR", export_def_bones=True,
        export_morph=False, export_lights=False, export_cameras=False)
    if transform:
        o.data.transform(transform.inverted())
    path = ASSETS / (col.name+".gltf")
    doc = json.loads(path.read_text())
    for mesh in doc["meshes"]:
        for primitive in mesh["primitives"]:
            count = doc["accessors"][primitive["attributes"]["POSITION"]]["count"]
            if count > 65535:
                raise RuntimeError("Primitive exceeds Unholy vertex limit: " + col.name)
    # Blender's default texture sampler is LINEAR minification. The engine
    # requires linear mipmaps: declare those for these explicitly tiled assets.
    for sampler in doc.get("samplers", []):
        sampler.update(magFilter=9729, minFilter=9987, wrapS=10497, wrapT=10497)
    for m in doc.get("materials", []):
        m.pop("doubleSided", None)  # All geometry is closed or explicitly backed.
        if m.get("extensions"):
            raise RuntimeError("Unexpected non-core material: " + str(m))
        if m["name"].startswith("Oak "):
            i = int(m["name"].split()[-1])
            m["pbrMetallicRoughness"]["baseColorFactor"] = [1-i*.045,1-i*.035,1-i*.025,1]
        if col.name == "key":
            m["emissiveFactor"] = [.08,.035,.005]
        if col.name == "exit" and m["name"] == "Exit enamel":
            m["emissiveFactor"] = [.015,.2,.045]
    doc["asset"]["generator"] = "Blender 5 / Unholy original Last Cue theatre workshop"
    path.write_text(json.dumps(doc, separators=(",", ":"))+"\n")
    print("EXPORTED", col.name, len(o.data.polygons), "polygons", flush=True)
    return o


def planks(width, depth, top, bottom, name="Board"):
    count = round(width/.45)
    w = width/count
    for i in range(count):
        start = -depth/2
        offset = (i % 3)*1.07
        cuts = [start] + [v for v in [start+offset+j*3.2 for j in range(12)] if start+.15 < v < depth/2-.15] + [depth/2]
        for j, (a,b) in enumerate(zip(cuts,cuts[1:])):
            x, z = -width/2+(i+.5)*w, (a+b)/2
            box(name, (x,(top+bottom)/2,z), (w-.012,top-bottom,b-a-.012), WOODS[(i+j)%5], .014, segments=1)
            if top-bottom > .1:
                for end in (a+.08,b-.08):
                    rod("Recessed nail", (x-.12,top-.003,end), (x-.12,top+.001,end), .011, IRON, 8)


if "--characters-only" not in sys.argv:
    collection("deck")
    box("Under-deck shadow", (0,0,0), (18,.78,26), IRON, 0)
    planks(18,26,.4,.28)
    export(current, (18,.8,26))

    for name, width, depth in [("wall",19,.6),("wing-wall",.6,28)]:
        collection(name)
        box("Mortar substrate", (0,0,0), (width,8,depth), MORTAR, 0)
        long = max(width,depth)
        for row in range(18):
            for i in range(math.ceil(long/.95)+1):
                a = -long/2 + i*.95 - (row%2)*.475
                b = min(long/2,a+.93)
                a = max(-long/2,a)
                if b <= a:
                    continue
                for side in [-1,1]:
                    center = ((a+b)/2,-4+(row+.5)*8/18,side*.302) if name == "wall" else (side*.302,-4+(row+.5)*8/18,(a+b)/2)
                    dims = (b-a,8/18-.025,.05) if name == "wall" else (.05,8/18-.025,b-a)
                    box("Hand laid brick",center,dims,random.choice(BRICKS),.009,segments=1)
        export(current,(width,8,depth))

    collection("key")
    ring("Bow outer bead", (0,.1,0), .24, .052, BRASS)
    ring("Bow inner bead", (0,.1,0), .145, .018, GOLD)
    for x in [-.17,.17]:
        sphere("Bow shoulder",(x,-.05,0),(.13,.13,.13),BRASS)
    rod("Key shank",(0,-.08,0),(0,-.7,0),.057,BRASS)
    for y in [-.35,-.42]:
        ring("Turned shank collar",(0,y,0),.074,.013,GOLD,axis="x")
    box("Key bit",(.12,-.64,0),(.25,.21,.10),BRASS,.016)
    box("Notched tooth",(.25,-.7,0),(.08,.1,.1),GOLD,.01)
    export(current)

    collection("winch")
    box("Oak machinery bed",(0,.1,0),(1.9,.2,1.25),WOODS[2],.04)
    for x in [-.68,.68]:
        box("Cast iron A frame",(x,.62,0),(.12,1,.60),IRON,.04)
        for z in [-.23,.23]:
            rod("Frame diagonal",(x,.2,z),(x,.98,0),.045,IRON)
        sphere("Shaft bearing",(x,1,0),(.24,.28,.28),BRASS)
    rod("Winch shaft",(-.85,1,0),(.93,1,0),.08,IRON)
    rod("Rope drum",(-.49,1,0),(.49,1,0),.29,IRON,32)
    for x in [-.5,.5]:
        rod("Drum flange",(x-.03,1,0),(x+.03,1,0),.37,BRASS,32)
    line("Spiral wound cable",[( -.46+.92*i/420, 1+.31*math.sin(i*math.tau/21), .31*math.cos(i*math.tau/21)) for i in range(421)],.021,ROPE)
    ring("Handwheel rim",(.87,1,0),.62,.045,IRON,axis="x")
    ring("Handwheel brass bead",(.875,1,0),.58,.018,BRASS,axis="x")
    for i in range(6):
        a = math.tau*i/6
        rod("Curved wheel spoke",(.87,1,0),(.87,1+.57*math.sin(a),.57*math.cos(a)),.027,IRON)
    rod("Crank handle",(.86,1.47,.3),(1.12,1.47,.3),.04,BRASS)
    sphere("Crank wooden grip",(1.13,1.47,.3),(.12,.15,.15),WOODS[1])
    box("Brake lever",(-.79,.8,.36),(.055,.7,.06),RUST,.01)
    sphere("Brake grip",(-.79,1.15,.36),(.14,.16,.12),IRON)
    for x in [-.8,.8]:
        for z in [-.5,.5]:
            rod("Anchor bolt",(x,.19,z),(x,.23,z),.045,BRASS,6)
    export(current)

    collection("pulley")
    for z in [-.15,.15]:
        box("Head block cheek",(0,0,z),(.88,.94,.075),WOODS[2],.12)
        for x in [-.29,.29]:
            box("Iron cheek strap",(x,0,z*1.35),(.075,.84,.03),IRON,.015)
            for y in [-.32,.32]:
                bolt((x,y,z*1.5))
    rod("Sheave axle",(0,0,-.24),(0,0,.24),.08,BRASS)
    ring("Grooved sheave",(0,0,0),.34,.07,IRON)
    ring("Suspension eye",(0,.65,0),.16,.034,IRON)
    box("Hanger",(0,.47,0),(.13,.3,.15),IRON)
    export(current)

    collection("flat")
    # Painted architectural scenic flat, with real stretcher frame and rear braces.
    box("Scenic panel",(0,2,0),(2.5,4,.10),TEAL,.015)
    for side in [-1,1]:
        z = side*.085
        for x in [-1.15,1.15]:
            box("Flat stile",(x,2,z),(.13,4,.1),WOODS[2])
        for y in [.09,1.7,3.91]:
            box("Flat stretcher",(0,y,z),(2.4,.13,.1),WOODS[2])
        # Double sided built scenery reads from either approach.
        box("Painted inset",(0,2.65,side*.14),(1.9,1.8,.05),IVORY)
        for x in [-.83,0,.83]:
            box("Scenic pilaster",(x,2.65,side*.18),(.075,1.85,.075),GOLD,.012)
        for y in [1.73,3.54]:
            box("Scenic cornice",(0,y,side*.18),(1.9,.09,.075),GOLD,.01)
        for x in [-.43,.43]:
            box("Painted window",(x,2.65,side*.19),(.55,1.28,.03),TEAL,.04)
            for y in [2.25,2.68,3.08]:
                box("Window crossbar",(x,y,side*.22),(.57,.035,.04),GOLD,.005)
        box("Painted dado",(0,.88,side*.145),(1.95,1.3,.05),RUST)
        for x in [-.69,0,.69]:
            box("Dado inset",(x,.88,side*.18),(.56,.95,.045),TEAL,.02)
    for x in [-1,1]:
        box("Stage flat foot",(x,.1,.4),(.23,.2,1.1),WOODS[3])
        rod("Rear folding brace",(x,.2,.87),(x,2.1,.18),.045,IRON)
    export(current)

    def drape(width, height, y0, z0, folds, sag=0):
        verts, faces = [], []
        nx, ny = folds*12, 24
        for j in range(ny+1):
            t = j/ny
            for i in range(nx+1):
                u = i/nx
                x = (u-.5)*width
                y = y0+t*height + sag*math.sin(math.pi*u)**2
                z = z0 + (.12+.04*(1-t))*math.cos(u*math.tau*folds)
                verts.append(xyz((x,y,z)))
        for j in range(ny):
            for i in range(nx):
                a = j*(nx+1)+i
                faces.append((a,a+1,a+nx+2,a+nx+1))
        mesh = bpy.data.meshes.new("Tailored velvet surface")
        mesh.from_pydata(verts,[],faces)
        mesh.uv_layers.new(name="UVMap")
        for poly in mesh.polygons:
            for loop in poly.loop_indices:
                v = mesh.loops[loop].vertex_index
                mesh.uv_layers.active.data[loop].uv = ((v%(nx+1))/nx,(v//(nx+1))/ny)
        o = bpy.data.objects.new("Pleated velvet",mesh)
        current.objects.link(o)
        active(o)
        mod = o.modifiers.new("Fabric lining", "SOLIDIFY")
        mod.thickness = .018
        bpy.ops.object.modifier_apply(modifier=mod.name)
        finish(o,"Pleated velvet",VELVET,True)
        line("Weighted gold hem",[( (i/nx-.5)*width, y0+sag*math.sin(math.pi*i/nx)**2+.04, z0+.16*math.cos(i/nx*math.tau*folds)) for i in range(nx+1)],.025,GOLD)


    collection("curtain")
    drape(2,7,.07,0,7)
    rod("Curtain rail",(-1.1,7.12,0),(1.1,7.12,0),.04,IRON)
    for i in range(14):
        ring("Curtain hanger",(-.93+i*.143,7.05,0),.065,.016,BRASS)
    export(current)

    collection("gate")
    drape(4.85,2.85,-1.42,0,12)
    rod("Weighted batten",(-2.5,-1.45,0),(2.5,-1.45,0),.055,IRON)
    box("Gate top batten",(0,1.45,0),(5,.1,.26),WOODS[3])
    export(current,(5,3,.4))

    collection("arch")
    for side in [-1,1]:
        x = side*7.3
        box("Proscenium timber column",(x,3.4,0),(.95,6.8,.75),TEAL,.09)
        for z in [-.42,.42]:
            for dx in [-.36,.36]:
                rod("Gilded fluting",(x+dx,.6,z),(x+dx,6.45,z),.045,GOLD)
            for y in [.2,.5,6.4,6.7]:
                box("Layered column molding",(x,y,z), (1.13,.13,.12),GOLD)
            for y in [1.5,3.2,4.9]:
                sphere("Column rosette",(x,y,z),(.28,.35,.08),BRASS)
            box("Column inset",(x,3.3,z),(.42,4.85,.05),RUST,.04)
        box("Column plinth",(x,.22,0),(1.3,.44,1),IRON,.06)
    box("Painted entablature",(0,7.05,0),(15.7,.72,.8),TEAL,.08)
    for z in [-.47,.47]:
        for y in [6.72,7.03,7.4]:
            box("Gilded cornice",(0,y,z),(16,.095,.15),GOLD)
        for i in range(39):
            box("Dentil carving",(-7.6+i*.4,7.27,z),(.14,.18,.12),BRASS)
        for h in [0,.12,.24]:
            line("Swept proscenium molding",[(7*math.cos(math.pi*i/80),5.25+(1.4+h)*math.sin(math.pi*i/80),z) for i in range(81)],.05,GOLD)
        sphere("Central cartouche",(0,6.83,z),(.65,.7,.12),GOLD)
        sphere("Cartouche enamel",(0,6.85,z*1.15),(.37,.44,.06),RUST)
    export(current)

    collection("counterweight")
    for x in [-.45,.45]:
        box("Steel guide",(x,2.5,0),(.065,5,.16),IRON)
    for h in [.25,.58,.91,1.24,1.57,1.9]:
        box("Cast weight plate",(0,h,0),(.84,.27,.46),IRON,.035)
        box("Plate painted end",(0,h,-.25),(.7,.16,.025),RUST,.01)
        bolt((-.24,h,-.28),.032)
        bolt((.24,h,-.28),.032)
    for h in [.09,2.12,4.9]:
        box("Arbor spreader",(0,h,0),(1,.12,.55),BRASS)
    rod("Operating line",(0,2.2,0),(0,4.85,0),.028,ROPE)
    ring("Upper head sheave",(0,4.65,0),.26,.04,IRON)
    export(current)

    for name,width,depth,thick in [("lift",4.4,3,.24),("gallery",16,4.6,.4)]:
        collection(name)
        box("Platform iron frame",(0,-.03,0),(width,thick-.06,depth),IRON,.03)
        planks(width,depth,thick/2,thick/2-.07)
        for x in [-width/2+.04,width/2-.04]:
            box("Brass edge protection",(x,thick/2-.015,0),(.06,.035,depth),BRASS,.01)
        for z in [-depth/2+.03,depth/2-.03]:
            box("Stage platform nosing",(0,thick/2-.015,z),(width,.035,.055),BRASS,.01)
        export(current,(width,thick,depth))

    collection("exit")
    for x in [-1.35,1.35]:
        box("Gallery portal jamb",(x,0,0),(.2,2,.35),TEAL,.04)
        box("Gallery portal trim",(x,0,.2),(.065,2,.05),GOLD,.01)
    box("Gallery portal lintel",(0,.95,0),(2.9,.18,.4),TEAL)
    box("Dark passage backing",(0,0,-.42),(2.8,2,.05),IRON,.01)
    GREEN = material("Exit enamel",(.055,.32,.20),0,.48)
    box("Exit header",(0,.65,.23),(1,.3,.08),GREEN)
    for side in [-1,1]:
        line("Exit direction chevron",[(side*.30,.60,.285),(side*.39,.65,.285),(side*.30,.70,.285)],.018,IVORY)
    box("Threshold",(0,-.97,0),(2.9,.07,.8),BRASS)
    export(current,(3,2,1))

    collection("worklight")
    rod("Industrial shade",(0,0,0),(0,-.23,0),.22,IRON,32)
    ring("Lamp brass rim",(0,-.23,0),.22,.025,BRASS)
    for x in [-.25,.25]:
        rod("Lamp yoke",(x,-.07,0),(x,.25,0),.027,IRON)
    rod("Lamp yoke bridge",(-.25,.25,0),(.25,.25,0),.027,IRON)
    export(current)

    # Space the workshop assets for a useful editable source file.
    for i, col in enumerate([c for c in bpy.data.collections if c.objects]):
        for o in col.objects:
            o.location += Vector((i%4*22,i//4*30,0))
    for im in bpy.data.images:
        if im.filepath:
            im.filepath = "//../assets/" + Path(im.filepath).name
    bpy.ops.wm.save_as_mainfile(filepath=str(SOURCE / "theatre-kit.blend"),compress=True)

# Character authoring follows below; use a separate file so the rig opens at
# human scale rather than being lost in the modular scenery workshop.
bpy.ops.object.select_all(action="SELECT")
bpy.ops.object.delete(use_global=False)
for col in list(bpy.data.collections):
    if col.name != "Collection":
        bpy.data.collections.remove(col)

COAT = material("Stagehand indigo twill",(.065,.15,.20),0,.88)
SEAM = material("Faded cloth seams",(.13,.24,.27),0,.93)
SKIN = material("Warm skin",(.57,.32,.18),0,.82)
HAIR = material("Dark hair",(.045,.024,.016),0,.9)
LEATHER = material("Worn boot leather",(.06,.038,.027),0,.62)
PANTS = material("Charcoal work trousers",(.075,.08,.09),0,.9)
SCARF = material("Ochre neckerchief",(.52,.28,.055),0,.9)
MASK = material("Performer porcelain mask",(.74,.66,.49),0,.34)
RED = material("Performer burgundy tailcoat",(.29,.035,.026),0,.7)


def loft(name, levels, mat, bone):
    """A tailored elliptical profile, with a continuous soft torso skin."""
    verts,faces = [],[]
    n = 24
    for y,rx,rz in levels:
        for i in range(n):
            a = math.tau*i/n
            verts.append(xyz((rx*math.cos(a),y,rz*math.sin(a))))
    for j in range(len(levels)-1):
        for i in range(n):
            faces.append((j*n+i,j*n+(i+1)%n,(j+1)*n+(i+1)%n,(j+1)*n+i))
    faces.extend([tuple(reversed(range(n))),tuple((len(levels)-1)*n+i for i in range(n))])
    mesh = bpy.data.meshes.new(name)
    mesh.from_pydata(verts,[],[tuple(reversed(f)) for f in faces])
    o = bpy.data.objects.new(name,mesh)
    current.objects.link(o)
    finish(o,name,mat,True,bone)
    if bone == "chest":
        hips = o.vertex_groups.new(name="hips")
        chest = o.vertex_groups["chest"]
        for v in mesh.vertices:
            weight = max(0,min(1,(v.co.z+.27)/.5))
            chest.add([v.index],weight,"REPLACE")
            hips.add([v.index],1-weight,"REPLACE")
    return o


def create_rig(col):
    arm = bpy.data.armatures.new("Stage performer skeleton")
    obj = bpy.data.objects.new("Rig",arm)
    col.objects.link(obj)
    active(obj)
    bpy.ops.object.mode_set(mode="EDIT")
    specs = [("hips",None,(0,-.36,0),(0,-.1,0)),("chest","hips",(0,-.1,0),(0,.46,0)),
             ("head","chest",(0,.46,0),(0,.97,0))]
    for s,side in [(-1,"L"),(1,"R")]:
        specs += [("arm."+side,"chest",(s*.36,.38,0),(s*.43,-.02,0)),
                  ("forearm."+side,"arm."+side,(s*.43,-.02,0),(s*.44,-.32,-.02)),
                  ("hand."+side,"forearm."+side,(s*.44,-.32,-.02),(s*.44,-.43,-.03)),
                  ("thigh."+side,"hips",(s*.16,-.35,0),(s*.16,-.65,0)),
                  ("shin."+side,"thigh."+side,(s*.16,-.65,0),(s*.16,-.95,0)),
                  ("foot."+side,"shin."+side,(s*.16,-.95,0),(s*.16,-.95,-.22))]
    for name,parent,head,tail in specs:
        b = arm.edit_bones.new(name)
        b.head,b.tail = xyz(head),xyz(tail)
        if parent:
            b.parent = arm.edit_bones[parent]
    bpy.ops.object.mode_set(mode="OBJECT")
    obj.show_in_front = True
    return obj


def animate(rig):
    rig.animation_data_create()
    scene = bpy.context.scene
    scene.render.fps = 30
    for name,duration in [("idle",90),("walk",30),("jump",30)]:
        rig.animation_data.action = bpy.data.actions.new(name)
        rig.animation_data.action.use_fake_user = True
        for frame in range(1,duration+2,3):
            if frame > duration+1:
                frame = duration+1
            t = (frame-1)/duration
            wave = math.sin(t*math.tau)
            for pb in rig.pose.bones:
                pb.rotation_mode = "XYZ"
                pb.rotation_euler = (0,0,0)
                pb.location = (0,0,0)
            if name == "idle":
                rig.pose.bones["chest"].rotation_euler.x = math.radians(1.2)*wave
                rig.pose.bones["head"].rotation_euler.z = math.radians(2)*wave
            elif name == "walk":
                for side,sign in [("L",1),("R",-1)]:
                    rig.pose.bones["thigh."+side].rotation_euler.x = math.radians(27)*wave*sign
                    rig.pose.bones["shin."+side].rotation_euler.x = -math.radians(30)*max(0,-wave*sign)
                    rig.pose.bones["arm."+side].rotation_euler.x = -math.radians(20)*wave*sign
                    rig.pose.bones["forearm."+side].rotation_euler.x = -math.radians(8)
                rig.pose.bones["hips"].location.y = -.02*(1-math.cos(t*math.tau*2))
                rig.pose.bones["chest"].rotation_euler.z = math.radians(3)*wave
            else:
                for side in ["L","R"]:
                    rig.pose.bones["thigh."+side].rotation_euler.x = math.radians(22)
                    rig.pose.bones["shin."+side].rotation_euler.x = -math.radians(42)
                    rig.pose.bones["arm."+side].rotation_euler.x = -math.radians(55)
                    rig.pose.bones["forearm."+side].rotation_euler.x = -math.radians(18)
            for pb in rig.pose.bones:
                pb.keyframe_insert(data_path="rotation_euler",frame=frame)
                pb.keyframe_insert(data_path="location",frame=frame)
        # Blender 5 stores F-curves in action channel bags.
        action = rig.animation_data.action
        for layer in action.layers:
            for strip in layer.strips:
                for bag in strip.channelbags:
                    for fc in bag.fcurves:
                        for key in fc.keyframe_points:
                            key.interpolation = "LINEAR"
    rig.animation_data.action = None
    for pb in rig.pose.bones:
        pb.rotation_euler = (0,0,0)
        pb.location = (0,0,0)
    scene.frame_set(1)


for mechanical in [False,True]:
    col = collection("performer" if mechanical else "stagehand")
    coat = RED if mechanical else COAT
    skin = MASK if mechanical else SKIN
    rig = create_rig(col)
    loft("Tailored work jacket",[(-.38,.32,.20),(-.29,.32,.20),(-.08,.27,.19),(.24,.32,.20),(.37,.34,.19),(.43,.28,.16),(.48,.115,.115)],coat,"chest")
    loft("Trouser seat",[(-.44,.25,.18),(-.3,.27,.18),(-.18,.24,.17)],PANTS,"hips")
    rod("Neck",(0,.44,0),(0,.61,0),.105,skin,24,"head")
    sphere("Sculpted head",(0,.74,-.005),(.39,.45,.37),skin,"head")
    for s in [-1,1]:
        sphere("Ear",(s*.194,.75,0),(.075,.13,.07),BRASS if mechanical else SKIN,"head")
        sphere("Deep eye socket",(s*.075,.79,-.176),(.065,.042,.03),IRON if mechanical else HAIR,"head")
        sphere("Eye glint",(s*.075,.795,-.193),(.012,.013,.011),RUST if mechanical else IVORY,"head")
        line("Expressive eyebrow",[(s*.04,.839,-.173),(s*.08,.85,-.173),(s*.12,.841,-.164)],.011,HAIR,"head")
    sphere("Defined nose",(0,.735,-.196),(.08,.1,.084),skin,"head")
    line("Mouth",[(-.058,.667,-.169),(0,.658,-.18),(.058,.667,-.169)],.009,HAIR,"head")
    if mechanical:
        sphere("Brass crown",(0,.96,0),(.43,.09,.36),BRASS,"head")
        ring("Performer collar",(0,.52,0),.13,.028,BRASS,"head",axis="x")
        # Visible automaton hardware and an unmistakable split tail silhouette.
        for x in [-.13,.13]:
            box("Tailcoat skirt",(x,-.43,.18),(.21,.38,.065),RED,.04,"hips")
        box("Clockwork back plate",(0,.17,.208),(.24,.33,.045),IRON,.02,"chest")
        for i in range(3):
            ring("Exposed clockwork wheel",((i-1)*.065,.18+(i%2)*.08,.244),.052,.011,BRASS,"chest")
    else:
        sphere("Hair under cap",(0,.86,.04),(.395,.20,.30),HAIR,"head")
        loft("Flat cap crown",[(.88,.21,.19),(.97,.215,.19),(1.035,.14,.13),(1.045,.02,.02)],COAT,"head")
        sphere("Cap brim",(0,.89,-.18),(.4,.045,.27),LEATHER,"head")
        line("Cap seam",[(-.17,.965,-.09),(0,1.047,-.03),(.17,.965,-.09)],.007,SEAM,"head")
        sphere("Neckerchief knot",(0,.49,-.115),(.13,.12,.10),SCARF,"chest")
        box("Scarf loose end",(.035,.37,-.21),(.08,.17,.035),SCARF,.018,"chest")
    # Clothing is built around the deforming rig, rather than floating ovals.
    for s,side in [(-1,"L"),(1,"R")]:
        rod("Jacket upper sleeve",(s*.35,.34,0),(s*.43,-.03,0),.099,coat,24,"arm."+side)
        sphere("Elbow cloth",(s*.43,-.025,0),(.184,.19,.18),coat,"forearm."+side)
        rod("Lower sleeve",(s*.43,-.02,0),(s*.44,-.28,-.018),.08,coat,24,"forearm."+side)
        box("Turned cuff",(s*.44,-.28,-.018),(.18,.07,.18),SEAM if not mechanical else BRASS,.025,"forearm."+side)
        sphere("Gloved hand",(s*.44,-.37,-.018),(.145,.18,.12),LEATHER if not mechanical else BRASS,"hand."+side)
        sphere("Thumb",(s*.38,-.35,-.055),(.065,.095,.075),LEATHER if not mechanical else BRASS,"hand."+side)
        rod("Work trouser thigh",(s*.16,-.34,0),(s*.16,-.66,0),.115,PANTS,24,"thigh."+side)
        rod("Tapered trouser calf",(s*.16,-.64,0),(s*.16,-.91,0),.088,PANTS,24,"shin."+side)
        box("Boot ankle",(s*.16,-.88,0),(.19,.19,.22),LEATHER,.045,"foot."+side)
        sphere("Shaped boot vamp",(s*.16,-.97,-.10),(.23,.13,.37),LEATHER,"foot."+side)
        box("Boot sole",(s*.16,-1.018,-.08),(.235,.04,.36),IRON,.025,"foot."+side)
        for y in [-.91,-.94,-.97]:
            line("Boot lace",[(s*.16-.046,y,-.14),(s*.16+.046,y,-.14)],.007,ROPE,"foot."+side)
        box("Jacket patch pocket",(s*.17,.015,-.202),(.18,.18,.035),coat,.02,"chest")
        line("Pocket seam",[(s*.25,.1,-.224),(s*.09,.1,-.224)],.006,SEAM,"chest")
        line("Back shoulder seam",[(s*.31,.36,.128),(s*.15,.30,.192),(0,.30,.202)],.006,SEAM,"chest")
    for y in [.29,.13,-.03]:
        bolt((0,y,-.214),.019,"chest")
    loft("Leather tool belt",[(-.255,.286,.214),(-.18,.286,.214)],LEATHER,"hips")
    box("Belt buckle",(0,-.21,-.23),(.095,.075,.035),BRASS,.009,"hips")
    if not mechanical:
        box("Belt tool pouch",(.23,-.33,.10),(.16,.23,.20),LEATHER,.045,"hips")
        rod("Hammer haft",(-.23,-.42,.23),(-.23,-.08,.23),.024,WOODS[2],12,"hips")
        box("Hammer iron head",(-.23,-.1,.23),(.17,.075,.07),IRON,.015,"hips")
        line("Braces left",[(-.18,-.17,.217),(-.12,.05,.211),(.18,.34,.168)],.014,LEATHER,"chest")
        line("Braces right",[(.18,-.17,.217),(.12,.05,.211),(-.18,.34,.168)],.014,LEATHER,"chest")
    mesh = join_meshes(col)
    mesh.parent = rig
    mod = mesh.modifiers.new("GPU skin export", "ARMATURE")
    mod.object = rig
    animate(rig)
    # Export already joined geometry with its skin, without joining a second time.
    export(col,rig=True)
    for im in bpy.data.images:
        if im.filepath:
            im.filepath = "//../assets/" + Path(im.filepath).name
    bpy.ops.wm.save_as_mainfile(filepath=str(SOURCE / (col.name+".blend")),compress=True)
    bpy.ops.object.select_all(action="SELECT")
    bpy.ops.object.delete(use_global=False)
    bpy.data.collections.remove(col)
    for action in list(bpy.data.actions):
        bpy.data.actions.remove(action)
print("Original theatre kit, stagehand and clockwork performer complete",flush=True)
