"""Original short game sounds, rendered to ordinary PCM WAVs. No runtime synthesis."""
from pathlib import Path
import math
import random
import struct
import wave

OUT=Path(__file__).resolve().parents[1]/'projects/marble-run/assets/audio'
OUT.mkdir(parents=True,exist_ok=True)
RATE=22050
random.seed(92)
def save(name,duration,signal):
    samples=[max(-1,min(1,signal(i/RATE))) for i in range(int(duration*RATE))]
    with wave.open(str(OUT/(name+'.wav')),'wb') as f:
        f.setnchannels(1);f.setsampwidth(2);f.setframerate(RATE)
        f.writeframes(b''.join(struct.pack('<h',round(v*32767)) for v in samples))
def bell(t,f,decay=3):
    return math.exp(-t*decay)*(math.sin(math.tau*f*t)+.28*math.sin(math.tau*f*2.01*t)+.1*math.sin(math.tau*f*3.98*t))*.45
save('jump',.35,lambda t:math.sin(math.tau*(320*t+700*t*t))*math.exp(-t*12)*.45)
save('land',.3,lambda t:(random.uniform(-1,1)*.5+math.sin(math.tau*115*t)*.5)*math.exp(-t*24)*.5)
save('fall',.7,lambda t:math.sin(math.tau*(500*t-260*t*t))*math.sin(math.pi*t/.7)*.25)
save('checkpoint',1.6,lambda t:sum(bell(t-d,f,4) for d,f in [(0,660),(.13,880),(.26,1320)] if t>=d)*.7)
save('finish',3,lambda t:sum(bell(t-d,f,2.2) for d,f in [(0,523.25),(.18,659.25),(.36,783.99),(.6,1046.5),(1,1318.5)] if t>=d)*.6)
save('start',1.3,lambda t:bell(t,440,4)+(.6*bell(t-.16,660,4) if t>.16 else 0))
# Wind and rolling loops have periodic envelopes; the seam is quiet.
save('wind',6,lambda t:(random.uniform(-1,1)*.32+math.sin(math.tau*71*t)*.08)*(.8+.2*math.sin(math.tau*t/6)))
save('roll',1,lambda t:(random.uniform(-1,1)*.3+math.sin(math.tau*95*t)*.14)*(0.75+.25*math.cos(math.tau*t*12)))
notes=[(0,261.625),(2,329.625),(4,391.995),(6,523.25),(8,391.995),(10,329.625)]
save('ambience',12,lambda t:sum(bell(t-d,f,1.1) for d,f in notes if t>=d)*.24*min(1,(12-t)*2))
print('Rendered 9 original WAV clips to',OUT)
