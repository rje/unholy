#include "raylib.h"
#include <stdio.h>
#include <string.h>
#include <math.h>

// raylib owns decoding, resampling, mixing and the platform audio device.
typedef struct { char path[4096]; Sound sound; int resume; } Clip;
static Clip clips[64];
static int clipCount, muted, paused;
static unsigned long mixedFrames;
static float mixedPeak;
static void meter(void *buffer,unsigned int frames) {
    const float *samples=buffer;
    mixedFrames+=frames;
    for(unsigned int i=0;i<frames*2;i++) if(fabsf(samples[i])>mixedPeak) mixedPeak=fabsf(samples[i]);
}
void hj_audio_init(void) {
    InitAudioDevice();
    if(IsAudioDeviceReady()) {SetMasterVolume(0.7f);AttachAudioMixedProcessor(meter);}
}
void hj_audio_toggle_mute(void) {
    muted=!muted;if(IsAudioDeviceReady()) SetMasterVolume(muted?0:0.7f);
}
void hj_audio_stop(void) {
    for(int i=0;i<clipCount;i++) {StopSound(clips[i].sound);clips[i].resume=0;}paused=0;
}
void hj_audio_pause(int pause) {
    if(pause==paused) return;
    paused=pause;
    for(int i=0;i<clipCount;i++) {
        if(pause) {clips[i].resume=IsSoundPlaying(clips[i].sound);if(clips[i].resume) PauseSound(clips[i].sound);}
        else if(clips[i].resume) {ResumeSound(clips[i].sound);clips[i].resume=0;}
    }
}
void hj_audio_close(void) {
    if(IsAudioDeviceReady()) {DetachAudioMixedProcessor(meter);printf("Audio mix: %lu frames, peak %.3f\n",mixedFrames,mixedPeak);}
    for(int i=0;i<clipCount;i++) UnloadSound(clips[i].sound);
    clipCount=0;
    if(IsAudioDeviceReady()) CloseAudioDevice();
}
int hj_audio_play(const char *path,float volume,float pitch,int loop) {
    if(!FileExists(path)) return 0;
    if(!IsAudioDeviceReady()) return 1; // Headless simulation has no audio device.
    int index=0;while(index<clipCount&&strcmp(clips[index].path,path)) index++;
    if(index==clipCount) {
        if(clipCount==64) return 0;
        Sound sound=LoadSound(path);if(!IsSoundValid(sound)) return 0;
        snprintf(clips[index].path,sizeof(clips[index].path),"%s",path);clips[index].sound=sound;clipCount++;
        printf("Audio loaded: %s\n",path);
    }
    SetSoundVolume(clips[index].sound,volume);
    SetSoundPitch(clips[index].sound,pitch);
    if(!paused && volume>0 && (!loop||!IsSoundPlaying(clips[index].sound))) PlaySound(clips[index].sound);
    return 1;
}
