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
#include <lua.h>
#include <lauxlib.h>
#include <lualib.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

// Window, widgets, input and Lua ABI only. Haskell owns the 3D renderer.
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

void hj_audio_init(void);
void hj_audio_close(void);
void hj_audio_stop(void);
void hj_audio_toggle_mute(void);
int hj_audio_play(const char *path,float volume,float pitch,int loop);

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
    hj_audio_init();
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
    hj_audio_close();
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
    if(key_pressed(KEY_M)) hj_audio_toggle_mute();
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
void hj_render_input(float *out) {
    Vector2 mouse=GetMousePosition(),delta=GetMouseDelta();
    out[0]=mouse.x;out[1]=mouse.y;out[2]=delta.x;out[3]=delta.y;
    out[4]=GetMouseWheelMove();out[5]=IsMouseButtonDown(MOUSE_BUTTON_RIGHT);
    out[6]=IsMouseButtonDown(MOUSE_BUTTON_MIDDLE);out[7]=mouse_pressed();out[8]=guiEnabled;
}

typedef struct {lua_State *L; int module; char message[2048]; char hud[2048]; char root[2048];} ScriptVM;
static ScriptVM *owner(lua_State *L) {return *(ScriptVM **)lua_getextraspace(L);}
static int script_print(lua_State *L) {
    ScriptVM *vm=owner(L);size_t used=strlen(vm->message);
    for(int i=1;i<=lua_gettop(L);i++) {
        size_t len;const char *s=luaL_tolstring(L,i,&len);
        if(used<sizeof(vm->message)-2) {int n=snprintf(vm->message+used,sizeof(vm->message)-used,"%s%s",i>1?" ":"",s);used+=n>0?(size_t)n:0;}
        lua_pop(L,1);
    }
    return 0;
}
static void instruction_limit(lua_State *L,lua_Debug *ar) {(void)ar;luaL_error(L,"Script exceeded its per-call instruction budget (200000)");}
void *hj_lua_load(const char *path,const char *root) {
    ScriptVM *vm=calloc(1,sizeof(*vm));
    if(!vm) return NULL;
    snprintf(vm->root,sizeof(vm->root),"%s",root);
    vm->L=luaL_newstate();if(!vm->L){free(vm);return NULL;}
    lua_State *L=vm->L;*(ScriptVM **)lua_getextraspace(L)=vm;
    luaL_openlibs(L);lua_pushcfunction(L,script_print);lua_setglobal(L,"print");
    lua_sethook(L,instruction_limit,LUA_MASKCOUNT,200000);
    if(luaL_loadfile(L,path)||lua_pcall(L,0,1,0)) {snprintf(vm->message,sizeof(vm->message),"%s",lua_tostring(L,-1));lua_pop(L,1);vm->module=LUA_NOREF;return vm;}
    if(!lua_istable(L,-1)) {snprintf(vm->message,sizeof(vm->message),"A script must return a table with start/update functions");lua_pop(L,1);vm->module=LUA_NOREF;return vm;}
    vm->module=luaL_ref(L,LUA_REGISTRYINDEX);return vm;
}
void hj_lua_close(void *ptr) {ScriptVM *vm=ptr;if(vm){lua_close(vm->L);free(vm);}}
const char *hj_lua_message(void *ptr) {return ((ScriptVM*)ptr)->message;}
const char *hj_lua_hud(void *ptr) {return ((ScriptVM*)ptr)->hud;}
void hj_lua_clear(void *ptr) {((ScriptVM*)ptr)->message[0]=0;}
int hj_lua_valid(void *ptr) {return ptr&&((ScriptVM*)ptr)->module!=LUA_NOREF;}
static void setnum(lua_State *L,const char *name,float n) {lua_pushnumber(L,n);lua_setfield(L,-2,name);}
static float getnum(lua_State *L,const char *name,float fallback) {
    lua_getfield(L,-1,name);int valid=0;float v=(float)lua_tonumberx(L,-1,&valid);lua_pop(L,1);return valid&&isfinite(v)?v:fallback;
}
static void setvec(lua_State *L,const char *name,const float *v) {lua_newtable(L);setnum(L,"x",v[0]);setnum(L,"y",v[1]);setnum(L,"z",v[2]);lua_setfield(L,-2,name);}
static void getvec(lua_State *L,const char *name,float *v) {lua_getfield(L,-1,name);if(lua_istable(L,-1)){v[0]=getnum(L,"x",v[0]);v[1]=getnum(L,"y",v[1]);v[2]=getnum(L,"z",v[2]);}lua_pop(L,1);}
static int scene_get(lua_State *L) {
    // scene:get accepts a stable entity ID or an entity name.
    lua_getfield(L,1,lua_isnumber(L,2)?"by_id":"by_name");lua_pushvalue(L,2);lua_gettable(L,-2);return 1;
}
static int scene_audio(lua_State *L) {
    const char *asset=luaL_checkstring(L,2);
    if(asset[0]=='/'||strstr(asset,"..")) return luaL_error(L,"Audio path must be project-relative");
    float volume=Clamp((float)luaL_optnumber(L,3,1),0,1),pitch=Clamp((float)luaL_optnumber(L,4,1),0.25f,3);
    char path[4096];snprintf(path,sizeof(path),"%s/%s",owner(L)->root,asset);
    int loop=lua_toboolean(L,lua_upvalueindex(1));
    if(!hj_audio_play(path,volume,pitch,loop)) return luaL_error(L,"Cannot load audio: %s",asset);
    return 0;
}
void hj_lua_world_begin(void *ptr,const float *cam) {
    ScriptVM *vm=ptr;lua_State *L=vm->L;lua_settop(L,0);
    lua_newtable(L);lua_newtable(L);lua_setfield(L,-2,"by_id");lua_newtable(L);lua_setfield(L,-2,"by_name");
    lua_pushcfunction(L,scene_get);lua_setfield(L,-2,"get");
    lua_pushboolean(L,0);lua_pushcclosure(L,scene_audio,1);lua_setfield(L,-2,"sound");
    lua_pushboolean(L,1);lua_pushcclosure(L,scene_audio,1);lua_setfield(L,-2,"loop");
    lua_newtable(L);setvec(L,"eye",cam);setvec(L,"target",cam+3);setnum(L,"fov",cam[6]);lua_setfield(L,-2,"camera");
    lua_setglobal(L,"scene");
}
void hj_lua_entity(void *ptr,int id,const char *name,const float *d,int hasBody) {
    lua_State *L=((ScriptVM*)ptr)->L;lua_getglobal(L,"scene");
    lua_newtable(L);setnum(L,"id",(float)id);lua_pushstring(L,name);lua_setfield(L,-2,"name");
    setvec(L,"position",d);setvec(L,"rotation",d+3);setvec(L,"scale",d+6);
    lua_newtable(L);setvec(L,"color",d+9);setnum(L,"metallic",d[12]);setnum(L,"roughness",d[13]);setnum(L,"emission",d[14]);lua_setfield(L,-2,"material");
    if(hasBody) {lua_newtable(L);setvec(L,"velocity",d+15);lua_pushboolean(L,d[18]>0.5f);lua_setfield(L,-2,"grounded");lua_setfield(L,-2,"body");}
    lua_getfield(L,-2,"by_id");lua_pushvalue(L,-2);lua_rawseti(L,-2,id);lua_pop(L,1);
    lua_getfield(L,-2,"by_name");lua_pushvalue(L,-2);lua_setfield(L,-2,name);lua_pop(L,3);
}
int hj_lua_call(void *ptr,int id,int start,float dt,const float *input) {
    ScriptVM *vm=ptr;lua_State *L=vm->L;
    if(vm->module==LUA_NOREF) return 1;
    lua_rawgeti(L,LUA_REGISTRYINDEX,vm->module);lua_getfield(L,-1,start?"start":"update");lua_remove(L,-2);
    if(lua_isnil(L,-1)){lua_pop(L,1);return 0;}
    if(!lua_isfunction(L,-1)){snprintf(vm->message,sizeof(vm->message),"Script callback must be a function");lua_pop(L,1);return 1;}
    lua_getglobal(L,"scene");lua_getfield(L,-1,"by_id");lua_rawgeti(L,-1,id);lua_remove(L,-2);lua_remove(L,-2);
    lua_pushnumber(L,dt);lua_newtable(L);setnum(L,"x",input[0]);setnum(L,"z",input[1]);
    lua_pushboolean(L,input[2]>0.5f);lua_setfield(L,-2,"jump");lua_pushboolean(L,input[3]>0.5f);lua_setfield(L,-2,"restart");
    lua_getglobal(L,"scene");
    lua_sethook(L,instruction_limit,LUA_MASKCOUNT,200000);
    if(lua_pcall(L,4,0,0)) {snprintf(vm->message,sizeof(vm->message),"%s",lua_tostring(L,-1));lua_pop(L,1);return 1;}
    lua_getglobal(L,"scene");lua_getfield(L,-1,"hud");
    if(lua_isstring(L,-1)) snprintf(vm->hud,sizeof(vm->hud),"%s",lua_tostring(L,-1));
    lua_pop(L,2);return 0;
}
void hj_lua_read_entity(void *ptr,int id,float *d,int hasBody) {
    lua_State *L=((ScriptVM*)ptr)->L;lua_getglobal(L,"scene");lua_getfield(L,-1,"by_id");lua_rawgeti(L,-1,id);
    if(lua_istable(L,-1)) {
        getvec(L,"position",d);getvec(L,"rotation",d+3);getvec(L,"scale",d+6);
        lua_getfield(L,-1,"material");if(lua_istable(L,-1)){getvec(L,"color",d+9);d[12]=getnum(L,"metallic",d[12]);d[13]=getnum(L,"roughness",d[13]);d[14]=getnum(L,"emission",d[14]);}lua_pop(L,1);
        if(hasBody) {lua_getfield(L,-1,"body");if(lua_istable(L,-1)) getvec(L,"velocity",d+15);lua_pop(L,1);}
    }
    lua_pop(L,3);
}
void hj_lua_read_camera(void *ptr,float *cam) {
    lua_State *L=((ScriptVM*)ptr)->L;lua_getglobal(L,"scene");lua_getfield(L,-1,"camera");
    if(lua_istable(L,-1)) {getvec(L,"eye",cam);getvec(L,"target",cam+3);cam[6]=getnum(L,"fov",cam[6]);}lua_pop(L,2);
}
