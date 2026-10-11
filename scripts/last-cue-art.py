#!/usr/bin/env python3
"""Regenerate Last Cue's original Blender kit, rigs, textures and PCM sounds."""
import math
import os
import shutil
import struct
import subprocess
import wave
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
ART = ROOT / "projects/last-cue/assets"
blender = os.environ.get("BLENDER") or shutil.which("blender")
if not blender and Path("/Applications/Blender.app/Contents/MacOS/Blender").exists():
    blender = "/Applications/Blender.app/Contents/MacOS/Blender"
if not blender:
    raise SystemExit("Install Blender 5 or set BLENDER to its executable.")
subprocess.run([blender, "-b", "--python-exit-code", "1", "--python", str(ROOT / "scripts/blender-last-cue.py")], check=True)
shutil.copyfile(ROOT / "projects/material-gallery/assets/studio.hdr", ART / "studio.hdr")

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
print('Generated Blender theatre kit, two animated characters and six WAV clips')
