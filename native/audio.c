#include "raylib.h"
#include <stdlib.h>
#include <math.h>
#include <stdatomic.h>

// Struct ABI and real-time meter only. All resource/playback policy is Haskell.
static atomic_ulong mixedFrames;
static _Atomic float mixedPeak;
static void meter(void *buffer,unsigned int frames) {
    const float *samples=buffer;
    float peak=atomic_load(&mixedPeak);
    for(unsigned int i=0;i<frames*2;i++) peak=fmaxf(peak,fabsf(samples[i]));
    atomic_store(&mixedPeak,peak);atomic_fetch_add(&mixedFrames,frames);
}
int hj_audio_init(void) {
    atomic_store(&mixedFrames,0);atomic_store(&mixedPeak,0);
    InitAudioDevice();if(!IsAudioDeviceReady()) return 0;
    AttachAudioMixedProcessor(meter);return 1;
}
void hj_audio_close(void) {if(IsAudioDeviceReady()) {DetachAudioMixedProcessor(meter);CloseAudioDevice();}}
unsigned long hj_audio_frames(void) {return atomic_load(&mixedFrames);}
float hj_audio_peak(void) {return atomic_load(&mixedPeak);}
void hj_audio_master(float volume) {SetMasterVolume(volume);}
void *hj_audio_load(const char *path) {
    Sound *sound=malloc(sizeof(*sound));if(!sound) return NULL;
    *sound=LoadSound(path);if(!IsSoundValid(*sound)) {free(sound);return NULL;}return sound;
}
void *hj_audio_alias(void *source) {
    Sound *sound=malloc(sizeof(*sound));if(!sound) return NULL;
    *sound=LoadSoundAlias(*(Sound *)source);
    if(!IsSoundValid(*sound)) {free(sound);return NULL;}return sound;
}
void hj_audio_unload(void *sound,int alias) {if(alias) UnloadSoundAlias(*(Sound *)sound);else UnloadSound(*(Sound *)sound);free(sound);}
void hj_audio_play(void *sound) {PlaySound(*(Sound *)sound);}
void hj_audio_stop(void *sound) {StopSound(*(Sound *)sound);}
void hj_audio_pause(void *sound) {PauseSound(*(Sound *)sound);}
void hj_audio_resume(void *sound) {ResumeSound(*(Sound *)sound);}
int hj_audio_playing(void *sound) {return IsSoundPlaying(*(Sound *)sound);}
void hj_audio_volume(void *sound,float value) {SetSoundVolume(*(Sound *)sound,value);}
void hj_audio_pitch(void *sound,float value) {SetSoundPitch(*(Sound *)sound,value);}
