#!/usr/bin/env python3
"""Bounded real-editor checks of public Haskell hook failure containment."""
import json
import os
from pathlib import Path
import subprocess
import tempfile

ROOT = Path(__file__).resolve().parents[1]
OUT = ROOT / 'build/verification/reliability'
OUT.mkdir(parents=True, exist_ok=True)
with tempfile.TemporaryDirectory(prefix='unholy-fault-') as temporary:
    executable = Path(temporary) / 'fault-check'
    subprocess.run(['cabal', 'exec', '--', 'ghc', '-threaded', '-package', 'unholy',
                    '-outputdir', temporary, str(ROOT / 'test/FaultGame.hs'), '-o', str(executable)], cwd=ROOT, check=True)
    for mode in ['initialize', 'before', 'after', 'hud', 'step']:
        project = json.loads((ROOT / 'projects/material-lab/project.hj.json').read_text())
        project.update(name='Fault check', haskellGame='fault-check', nextId=3)
        project['entities'] = [dict(id=1, name='Edited cube', transform=dict(position=[0,.5,0], rotation=[0,0,0], scale=[1,1,1]),
                                   mesh=dict(geometry='cube',texture=''), material=dict(albedo=[.2,.6,.9],metallic=.4,roughness=.3,emission=0),
                                   body=dict(shape='box',mass=0,velocity=[0,0,0],restitution=.1,friction=.5)),
                               dict(id=2,name='Trim',transform=dict(position=[0,1.2,0],rotation=[0,0,0],scale=[.25,.25,.25]),
                                    mesh=dict(geometry='sphere',texture=''),material=dict(albedo=[1,.6,.2],metallic=.8,roughness=.2,emission=0),
                                    parent=dict(entity=1,local=dict(position=[0,.7,0],rotation=[0,0,0],scale=[.25,.25,.25])))]
        folder = OUT / mode
        folder.mkdir(exist_ok=True)
        path = folder / 'project.hj.json'
        Path(str(path)+'.recovery').unlink(missing_ok=True)
        path.write_text(json.dumps(project))
        events = ['e 10 7 713 415 0 0','e 11 6 0 0 0 0','e 20 7 743 428 0 0','e 30 5 0 0 0 0']
        def click(frame,x,y):
            events.extend([f'e {frame} 7 {x} {y} 0 0',f'e {frame+1} 6 0 0 0 0',f'e {frame+4} 5 0 0 0 0'])
        click(40,1165,214)  # Duplicate the assembly: a second unsaved edit.
        if mode == 'step':
            click(70,550,31)
            click(75,646,31)
            events.append('e 87 2 69 0 0 0')
            click(88,719,31)
            events.append('e 94 1 69 0 0 0')
        else:
            click(70,550,31)
        click(105,550,31)  # Second Play must be healthy after the one-shot failure.
        replay=folder/'fault.rae'
        replay.write_text(f'c {len(events)}\n'+'\n'.join(events)+'\n')
        result=subprocess.run([str(executable),str(path),'--engine-assets',str(ROOT/'assets'),
                               '--replay',str(replay),'--frames','150','--capture',str(folder/'editor.png')],
                              cwd=temporary,env=dict(os.environ,UNHOLY_FAULT_MODE=mode),capture_output=True,text=True,timeout=30)
        (folder/'run.log').write_text(result.stdout+result.stderr)
        assert result.returncode==0, f'{mode}: editor exited with {result.returncode}'
        assert result.stdout.count('FAULT-CHECK initialize')==2, f'{mode}: editor did not restart Play after failure'
        recovered=json.loads(Path(str(path)+'.recovery').read_text())
        x=recovered['entities'][0]['transform']['position'][0]
        assert .5<x<1.5, f'{mode}: unsaved gizmo edit lost: {x}'
        assert len(recovered['entities'])==4, f'{mode}: unsaved assembly duplication lost'
        assert recovered['entities'][3]['parent']['entity']==3, f'{mode}: duplicate child lost its parent'
        assert json.loads(path.read_text())['entities'][0]['transform']['position'][0]==0
        print(f'PASS {mode}: editor survives and restarts; unsaved assembly recovered (x={x:.3f}, 4 entities)')
