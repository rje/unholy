"""Aether Gardens scene composition; imported by generate-samples.py."""
import math

def build_course(entity,body):
    stone='assets/textures/temple-stone.png'
    model=lambda name:'assets/models/'+name+'.obj'
    brass=(.82,.59,.27);teal=(.12,.76,.89);ivory=(.96,.94,.88)
    course=[entity('Marble','sphere',(0,1,8),(1,1,1),(1,1,1),.25,.13,
        script='assets/scripts/marble.lua',body=body('sphere',1,.55),texture='assets/textures/porcelain-marble.png')]
    def add(name,shape,pos,size,color=ivory,metal=0,rough=.55,**kw):
        course.append(entity(name,shape,pos,size,color,metal,rough,**kw))
    def slab(name,pos,size,shape='slab',color=ivory,**kw):
        add(name,model(shape),pos,size,color,texture=stone,**kw)
    platforms=[('Start island',(0,-.5,8),(7,1,6),'start'),
        ('Blue bridge / approach',(0,-.5,3),(3.2,1,4),'approach'),
        ('Blue bridge / landing',(0,-.5,-1.75),(3.2,1,3.5),'landing'),
        ('Checkpoint island',(0,-.5,-5),(6,1,4),'checkpoint'),
        ('Turn right',(5,-.5,-5),(4.4,1,4),'shuttle'),
        ('Finish island',(9,-.5,-5),(4,1,6),'finish'),
        ('Sky lift',(-6,-.5,8),(3,1,3),'lift')]
    for name,pos,size,shape in platforms:
        x,y,z=pos
        slab(name,pos,size,shape,body=body(kinematic=name in ('Turn right','Sky lift')))
        prefix='Shuttle / ' if name=='Turn right' else 'Lift / ' if name=='Sky lift' else name+' / '
        add(prefix+'brass hull',model(shape),(x,y-.28,z),(size[0]*1.04,.28,size[2]*1.04),brass,.72,.28)
        add(prefix+'lower rim',model(shape),(x,y-.65,z),(size[0]*.93,.22,size[2]*.93),(.11,.24,.28),.55,.35)
        if name not in ('Turn right','Sky lift'):
            add(name+' / hanging rock',model('island'),(x,-2.3,z),(size[0]*.85,3.5,size[2]*.85),(.50,.57,.53),texture=stone,rot=(0,17,0))
        for side in (-1,1):
            add(prefix+f'edge {side}',model('slab'),(x+side*(size[0]/2-.1),.04,z),(.13,.11,size[2]-.4),brass,.7,.28)
            add(prefix+f'guide {side}',model('slab'),(x+side*(size[0]/2-.24),.08,z),(.055,.05,size[2]-.55),teal,.1,.25,emission=1.6)
    slab('Lift boarding bridge',(-4.1,-.5,8),(2.5,1,2.3),body=body())
    for side in (-1,1):
        for z,length,label in [(3,3.5,'Approach'),(-1.6,2.8,'Landing')]:
            slab(f'{label} guard {side}',(side*1.55,.17,z),(.14,.34,length),color=(.78,.84,.84),body=body())
        for z in (5.25,10.75):
            slab(f'Start rail {side} {z}',(side*3.46,.22,z),(.16,.44,.65),body=body())
    for i,z in enumerate((9,7.7,6.3,4.7,3.5,2.3,-.55,-1.8,-3.2)):
        add(f'Route dash {i}',model('slab'),(0,.035,z),(.16,.045,.55),teal,.1,.25,emission=.7)
    for z in (1.15,-.13):
        add('Jump lip '+str(z),model('slab'),(0,.04,z),(2.7,.07,.13),brass,.65,.27)
    add('Checkpoint beacon','torus',(0,.12,-5),(1.9,1.9,1.9),(.25,.71,.68),.25,.3,rot=(90,0,0),emission=.5)
    slab('Checkpoint medallion',(0,-.02,-5),(2.4,.14,2.4),color=(.57,.83,.79))
    add('Finish ring','torus',(9,1.4,-5.7),(2.4,2.4,2.4),brass,.72,.18,emission=.3)
    slab('Finish arch',(9,1.4,-7.05),(1.8,2.8,1.1),'gateway')
    add('Finish arch / gold inset',model('gateway'),(9,1.4,-6.83),(1.72,2.68,.48),brass,.75,.24)
    for x,z in ((-2.7,6), (2.7,6),(-2.45,-6.2),(2.45,-6.2),(7.6,-7.1),(10.4,-7.1)):
        slab(f'Temple pillar {x} {z}',(x,1.4,z),(.65,2.8,.65),'pillar')
        add(f'Pillar collar {x} {z}','torus',(x,1.36,z),(.50,.50,.50),brass,.8,.25,rot=(90,0,0))
        add(f'Lantern {x} {z}','sphere',(x,2.95,z),(.22,.22,.22),teal,.1,.25,emission=2.4)
    for x,z in ((-2.3,-4.9),(2.3,-4.9),(-6.85,8.9),(-5.15,8.9)):
        prefix='Lift / ' if x<-3 else 'Checkpoint / '
        add(prefix+f'crystal {x}',model('crystal'),(x,.7,z),(.35,1.1,.35),(.23,.87,.96),.12,.2,emission=.55)
    slab('Lift upper balcony',(-9.1,2.7,8),(3,1,4),body=body())
    add('Lift balcony / island',model('island'),(-9.1,.4,8),(4,3.2,4.5),(.45,.56,.50),texture=stone)
    slab('Balcony sweeper',(-9.1,3.6,8),(2.4,.22,.18),color=brass,body=body(kinematic=True),metal=.6,rough=.3)
    for i,(x,y,z,s) in enumerate([(-14,-7,-5,10),(17,-9,3,12),(-12,-13,-23,16),
        (18,-14,-25,17),(3,-19,-40,23),(-31,-19,-34,26),(37,-23,-44,30)]):
        add(f'Distant island {i}',model('island'),(x,y,z),(s,s*.8,s),(.45,.61,.60),texture=stone,rot=(0,i*43,0))
        for j in range(3):
            a=j*math.tau/3
            px=x+math.cos(a)*s*.18;pz=z+math.sin(a)*s*.18;py=y+s*.8*.35
            slab(f'Garden pillar {i} {j}',(px,py+1.2,pz),(.7,3,.7),'pillar')
            add(f'Garden canopy {i} {j}',model('canopy'),(px,py+3,pz),(2.5,2.7,2.5),(.20,.44,.36),rough=.9)
    for i,(x,z) in enumerate(((-2.8,9.5),(2.8,9.5),(-2.35,-6.1),(10.1,-3.3))):
        slab(f'Planter {i}',(x,.16,z),(.8,.35,.8),color=(.70,.80,.75))
        add(f'Garden shrub {i}',model('canopy'),(x,.6,z),(1,.9,1),(.22,.47,.34),rough=.92)
    for i,(x,z,color) in enumerate(((0,8,(.38,.74,1)),(0,-5,(.35,1,.73)),(9,-5,(1,.73,.36)))):
        add(f'Course light {i}',None,(x,4,z),(1,1,1))
        course[-1]['light']=dict(color=color,intensity=35)
    add('Checkpoint trigger',None,(0,1,-5),(6,2,4),body=body(trigger=True))
    add('Finish trigger',None,(9,1,-5),(3.5,2,5),body=body(trigger=True))
    env=dict(sunDirection=[-.55,-1,-.32],sunColor=[1,.88,.69],sunPower=3.3,ambient=.24,exposure=1,
        skyTop=[.27,.55,.72],skyBottom=[.80,.88,.84],fogDensity=.009,sunShadows=True)
    return course,[11,13,21],[0,0,3],env
