#include "raylib.h"
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
// raygui consumes the Haskell frame snapshot. These are ABI event buffers,
// not focus, binding, clipping, replay timing or theme policy.
static unsigned char widgetKeys[512];
static int widgetPressed,widgetReleased;
#define GUI_BUTTON_PRESSED widgetPressed
#define GUI_BUTTON_RELEASED widgetReleased
#define GUI_KEY_PRESSED(key) (widgetKeys[(key)])
#define RAYGUI_IMPLEMENTATION
#include "vendor/raygui.h"
#ifdef __APPLE__
void hj_platform_init(void *handle);
void hj_platform_poll(int *out);
void hj_platform_close(void);
#else
static void hj_platform_init(void *handle) {(void)handle;}
static void hj_platform_poll(int *out) {out[0]=out[1]=0;}
static void hj_platform_close(void) {}
#endif
static Font font;
static int ownFont;
static char nativeError[1024];
int hj_init(const char *title,const char *fontPath,int width,int height,int minWidth,int minHeight,int fps,unsigned int flags) {
    if(IsWindowReady()) return 0;
    SetTraceLogLevel(LOG_WARNING);SetConfigFlags(flags);InitWindow(width,height,title);
    if(!IsWindowReady()) return 0;
    hj_platform_init(GetWindowHandle());SetWindowMinSize(minWidth,minHeight);
    SetExitKey(KEY_NULL);SetTargetFPS(fps);SetGesturesEnabled(GESTURE_TAP|GESTURE_DOUBLETAP);
    font=GetFontDefault();ownFont=0;
    if(FileExists(fontPath)) {font=LoadFontEx(fontPath,40,NULL,0);ownFont=1;SetTextureFilter(font.texture,TEXTURE_FILTER_BILINEAR);}
    GuiLoadStyleDefault();GuiSetFont(font);return 1;
}
void hj_close(void) {hj_platform_close();if(ownFont)UnloadFont(font);CloseWindow();}
int hj_should_close(void) {return WindowShouldClose();}
int hj_width(void) {return GetScreenWidth();}
int hj_height(void) {return GetScreenHeight();}
float hj_dt(void) {return GetFrameTime();}
void hj_input(int *down,int *pressed,float *out) {
    for(int i=0;i<512;i++){down[i]=IsKeyDown(i);pressed[i]=IsKeyPressed(i);}
    int k;while((k=GetKeyPressed())>0)if(k<512)pressed[k]=1;
    int edges[2];hj_platform_poll(edges);
    Vector2 p=GetMousePosition(),d=GetMouseDelta();
    float raw[]={p.x,p.y,d.x,d.y,GetMouseWheelMove(),IsMouseButtonDown(1),IsMouseButtonDown(2),IsMouseButtonPressed(0),IsMouseButtonDown(0),IsMouseButtonReleased(0),edges[0],edges[1],IsGestureDetected(GESTURE_TAP)};
    memcpy(out,raw,sizeof(raw));
}
void hj_gamepad(int *buttons,float *axes) {
    int available=IsGamepadAvailable(0);buttons[0]=available;
    for(int i=0;i<18;i++) buttons[i+1]=available?IsGamepadButtonDown(0,i):0;
    axes[0]=available?GetGamepadAxisMovement(0,GAMEPAD_AXIS_LEFT_X):0;
    axes[1]=available?GetGamepadAxisMovement(0,GAMEPAD_AXIS_LEFT_Y):0;
    axes[2]=available?GetGamepadAxisMovement(0,GAMEPAD_AXIS_RIGHT_X):0;
    axes[3]=available?GetGamepadAxisMovement(0,GAMEPAD_AXIS_RIGHT_Y):0;
}
void hj_widget_input(const int *keys,int pressed,int released) {for(int i=0;i<512;i++)widgetKeys[i]=keys[i]!=0;widgetPressed=pressed;widgetReleased=released;}
void *hj_replay_load(const char *path) {AutomationEventList *list=calloc(1,sizeof(*list));if(list)*list=LoadAutomationEventList(path);return list;}
void hj_replay_free(void *p) {if(p){UnloadAutomationEventList(*(AutomationEventList*)p);free(p);}}
unsigned int hj_replay_count(void *p) {return ((AutomationEventList*)p)->count;}
unsigned int hj_replay_frame(void *p,unsigned int i) {return ((AutomationEventList*)p)->events[i].frame;}
void hj_replay_event(void *p,unsigned int i) {PlayAutomationEvent(((AutomationEventList*)p)->events[i]);}
void hj_begin(unsigned int color) {BeginDrawing();ClearBackground(GetColor(color));}
void hj_end(void) {EndDrawing();}
void hj_screenshot(const char *path) {Image image=LoadImageFromScreen();if(!ExportImage(image,path))snprintf(nativeError,sizeof(nativeError),"Cannot save screenshot: %s",path);UnloadImage(image);}
const char *hj_error(void) {return nativeError;}
void hj_clear_error(void) {nativeError[0]=0;}
void hj_style(int control,int property,unsigned int value) {GuiSetStyle(control,property,(int)value);}
void hj_widget_enabled(int enabled) {if(enabled)GuiUnlock();else GuiLock();}
void hj_rect(float x,float y,float w,float h,unsigned int color) {DrawRectangleRec((Rectangle){x,y,w,h},GetColor(color));}
void hj_text(float x,float y,float size,const char *s,unsigned int color) {DrawTextEx(font,s,(Vector2){x,y},size,0.5f,GetColor(color));}
float hj_measure(const char *s,float size) {return MeasureTextEx(font,s,size,0.5f).x;}
int hj_button(float x,float y,float w,float h,const char *label) {return GuiButton((Rectangle){x,y,w,h},label);}
float hj_slider(float x,float y,float w,float value,float lo,float hi) {GuiSliderBar((Rectangle){x,y,w,14},NULL,NULL,&value,lo,hi);return value;}
int hj_textbox(float x,float y,float w,float h,char *buffer,int capacity,int editing) {return GuiTextBox((Rectangle){x,y,w,h},buffer,capacity,editing!=0);}
void hj_clip(float x,float y,float w,float h) {BeginScissorMode((int)x,(int)y,(int)w,(int)h);}
void hj_unclip(void) {EndScissorMode();}
