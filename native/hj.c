#include "raylib.h"
#include "raymath.h"
#include "rlgl.h"
static int mouse_pressed(void);
static int key_pressed(int key);
#define GUI_BUTTON_PRESSED mouse_pressed()
static int mouse_released(void);
#define GUI_BUTTON_RELEASED mouse_released()
#define GUI_KEY_PRESSED(key) key_pressed(key)
#define RAYGUI_IMPLEMENTATION
#include "vendor/raygui.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

// Window, widgets and input ABI only. Haskell owns engine policy.
static Font font;
static int ownFont, textFocus=-1, guiEnabled=1, clipped=0;
static char editBuffer[2048];
static unsigned char pressedKeys[512];
static int clickEdges[2];
#ifdef __APPLE__
void hj_platform_init(void *handle);
void hj_platform_poll(int *out);
void hj_platform_close(void);
#else
static void hj_platform_init(void *handle) {(void)handle;}
static void hj_platform_poll(int *out) {out[0]=out[1]=0;}
static void hj_platform_close(void) {}
#endif
static Rectangle clipRect;
static char nativeError[1024];
static AutomationEventList replay;
static unsigned int replayFrame,replayIndex;
static const Color bg={27,28,31,255}, panel={38,39,43,255}, border={57,59,65,255};
static const Color ink={218,221,229,255}, muted={149,153,165,255}, accent={98,161,246,255};

static void txt(const char *s,float x,float y,float size,Color color) {
    DrawTextEx(font,s,(Vector2){x,y},size,0.5f,color);
}
int hj_init(const char *title,const char *assetRoot) {
    (void)assetRoot;
    SetTraceLogLevel(LOG_WARNING);
    SetConfigFlags(FLAG_WINDOW_RESIZABLE|FLAG_WINDOW_HIGHDPI|FLAG_MSAA_4X_HINT|FLAG_VSYNC_HINT);
    InitWindow(1440,900,title);
    if (!IsWindowReady()) return 0;
    hj_platform_init(GetWindowHandle());
    SetWindowMinSize(1100,760);
    SetExitKey(KEY_NULL);
    SetTargetFPS(60);
    SetGesturesEnabled(GESTURE_TAP|GESTURE_DOUBLETAP);
    font=GetFontDefault();
#ifdef __APPLE__
    const char *fontPath="/System/Library/Fonts/Supplemental/Arial.ttf";
#else
    const char *fontPath="/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf";
#endif
    if (FileExists(fontPath)) { font=LoadFontEx(fontPath,40,NULL,0); ownFont=1; SetTextureFilter(font.texture,TEXTURE_FILTER_BILINEAR); }
    GuiLoadStyleDefault();
    GuiSetFont(font);
    GuiSetStyle(DEFAULT,TEXT_SIZE,15);
    GuiSetStyle(DEFAULT,TEXT_SPACING,0);
    GuiSetStyle(DEFAULT,BORDER_WIDTH,1);
    GuiSetStyle(DEFAULT,BORDER_COLOR_NORMAL,ColorToInt(border));
    GuiSetStyle(DEFAULT,BASE_COLOR_NORMAL,ColorToInt(panel));
    GuiSetStyle(DEFAULT,TEXT_COLOR_NORMAL,ColorToInt(ink));
    GuiSetStyle(DEFAULT,BORDER_COLOR_FOCUSED,ColorToInt(accent));
    GuiSetStyle(DEFAULT,BASE_COLOR_FOCUSED,0x363B46FF);
    GuiSetStyle(DEFAULT,TEXT_COLOR_FOCUSED,ColorToInt(ink));
    GuiSetStyle(DEFAULT,BORDER_COLOR_PRESSED,ColorToInt(accent));
    GuiSetStyle(DEFAULT,BASE_COLOR_PRESSED,0x34567FFF);
    GuiSetStyle(DEFAULT,TEXT_COLOR_PRESSED,0xFFFFFFFF);
    GuiSetStyle(TEXTBOX,BASE_COLOR_NORMAL,ColorToInt(bg));
    GuiSetStyle(TEXTBOX,TEXT_PADDING,7);
    GuiSetStyle(BUTTON,TEXT_ALIGNMENT,TEXT_ALIGN_LEFT);
    GuiSetStyle(BUTTON,TEXT_PADDING,10);
    GuiSetStyle(SLIDER,SLIDER_PADDING,0);
    return 1;
}
void hj_close(void) {
    hj_platform_close();
    if(replay.events) UnloadAutomationEventList(replay);
    if(ownFont) UnloadFont(font);
    CloseWindow();
}
int hj_should_close(void) { return WindowShouldClose(); }
int hj_width(void) { return GetScreenWidth(); }
int hj_height(void) { return GetScreenHeight(); }
float hj_dt(void) {
    while(replayIndex<replay.count&&replay.events[replayIndex].frame<=replayFrame) PlayAutomationEvent(replay.events[replayIndex++]);
    replayFrame++;
    memset(pressedKeys,0,sizeof(pressedKeys));
    int k;while((k=GetKeyPressed())>0) if(k<512) pressedKeys[k]=1;
    hj_platform_poll(clickEdges);
    return GetFrameTime();
}
int hj_replay(const char *path) {replay=LoadAutomationEventList(path);replayFrame=replayIndex=0;return replay.count>0;}
static int mouse_pressed(void) {return clickEdges[0]||IsMouseButtonPressed(MOUSE_BUTTON_LEFT)||IsGestureDetected(GESTURE_TAP);}
static int mouse_released(void) {return clickEdges[1]||IsMouseButtonReleased(MOUSE_BUTTON_LEFT);}
static int key_pressed(int key) {return IsKeyPressed(key)||(key>=0&&key<512&&pressedKeys[key]);}
void hj_begin(void) {BeginDrawing();ClearBackground(bg);}
void hj_end(void) { EndDrawing(); }
void hj_screenshot(const char *path) {
    Image frame=LoadImageFromScreen();
    if(!ExportImage(frame,path)) snprintf(nativeError,sizeof(nativeError),"Cannot save screenshot: %s",path);
    UnloadImage(frame);
}
int hj_key(int key,int pressed) { return textFocus<0 && (pressed?key_pressed(key):IsKeyDown(key)); }
void hj_clear_focus(void) { textFocus=-1; }
void hj_ui_enabled(int enabled) {guiEnabled=enabled;}
void hj_inputs(float *out) {
    int disabled=textFocus>=0;
    out[0]=disabled?0:(IsKeyDown(KEY_D)||IsKeyDown(KEY_RIGHT))-(IsKeyDown(KEY_A)||IsKeyDown(KEY_LEFT));
    out[1]=disabled?0:(IsKeyDown(KEY_S)||IsKeyDown(KEY_DOWN))-(IsKeyDown(KEY_W)||IsKeyDown(KEY_UP));
    out[2]=!disabled&&IsKeyDown(KEY_SPACE);
    out[3]=!disabled&&IsKeyDown(KEY_R);
    out[4]=!disabled&&IsKeyDown(KEY_E);
}
const char *hj_error(void) { return nativeError; }
void hj_clear_error(void) { nativeError[0]=0; }
void hj_rect(float x,float y,float w,float h,int style) {
    DrawRectangleRec((Rectangle){x,y,w,h},style==1?panel:style==2?border:style==3?accent:bg);
}
void hj_text(float x,float y,float size,const char *s,int style) {
    txt(s,x,y,size,style==1?muted:style==2?accent:style==3?(Color){244,153,131,255}:ink);
}
static int interaction(void) {
    return guiEnabled && (!clipped || CheckCollisionPointRec(GetMousePosition(),clipRect));
}
static void gui_interaction(void) { if(interaction())GuiUnlock();else GuiLock(); }
int hj_button(float x,float y,float w,float h,const char *label,int style) {
    gui_interaction();
    GuiSetStyle(BUTTON,BASE_COLOR_NORMAL,style==1?0x365782FF:style==2?0x523B40FF:style==3?0x354F70FF:ColorToInt(panel));
    GuiSetStyle(BUTTON,BORDER_COLOR_NORMAL,style==1?0x5087CAFF:ColorToInt(border));
    return GuiButton((Rectangle){x,y,w,h},label);
}
float hj_slider(float x,float y,float w,const char *label,float value,float lo,float hi) {
    txt(label,x,y,14,muted);
    const char *val=TextFormat("%.2f",value);
    float width=MeasureTextEx(font,val,14,0.5f).x;
    txt(val,x+w-width,y,14,ink);
    gui_interaction();
    GuiSliderBar((Rectangle){x,y+21,w,14},NULL,NULL,&value,lo,hi);
    return value;
}
int hj_textbox(int id,float x,float y,float w,float h,char *buffer,int capacity) {
    Rectangle r={x,y,w,h};
    if(guiEnabled && mouse_pressed()) {
        if(interaction() && CheckCollisionPointRec(GetMousePosition(),r)) {
            if(textFocus!=id) snprintf(editBuffer,sizeof(editBuffer),"%s",buffer);
            textFocus=id;
        }
        else if(textFocus==id) textFocus=-1;
    }
    gui_interaction();
    if(textFocus==id && guiEnabled) {
        // Keep incomplete numeric input (e.g. '-' or '.') across Haskell frames.
        if(GuiTextBox(r,editBuffer,sizeof(editBuffer),true)) textFocus=-1;
        snprintf(buffer,(size_t)capacity,"%s",editBuffer);
    } else GuiTextBox(r,buffer,capacity,false);
    return 1;
}
float hj_wheel(float x,float y,float w,float h) {
    return guiEnabled&&CheckCollisionPointRec(GetMousePosition(),(Rectangle){x,y,w,h})?GetMouseWheelMove():0;
}
void hj_clip(float x,float y,float w,float h) { clipRect=(Rectangle){x,y,w,h};clipped=1;BeginScissorMode((int)x,(int)y,(int)w,(int)h); }
void hj_unclip(void) { clipped=0;EndScissorMode(); }
// Flat mouse snapshot, including replay/platform click edges used by the UI.
int hj_left_down(void) { return IsMouseButtonDown(MOUSE_BUTTON_LEFT); }
void hj_render_input(float *out) {
    Vector2 mouse=GetMousePosition(),delta=GetMouseDelta();
    out[0]=mouse.x;out[1]=mouse.y;out[2]=delta.x;out[3]=delta.y;
    out[4]=GetMouseWheelMove();out[5]=IsMouseButtonDown(MOUSE_BUTTON_RIGHT);
    out[6]=IsMouseButtonDown(MOUSE_BUTTON_MIDDLE);out[7]=mouse_pressed();out[8]=guiEnabled;
}
