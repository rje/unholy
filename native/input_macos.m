#import <Cocoa/Cocoa.h>

// Preserve short clicks that begin and end between raylib's frame polls.
// Events still flow through GLFW normally; this only latches their edges.
static id monitor;
static int pressed, released;
void hj_platform_init(void *handle) {
    NSWindow *window=(NSWindow *)handle;
    monitor=[[NSEvent addLocalMonitorForEventsMatchingMask:(NSEventMaskLeftMouseDown|NSEventMaskLeftMouseUp)
        handler:^NSEvent *(NSEvent *event) {
            if(event.window==window) {
                if(event.type==NSEventTypeLeftMouseDown) pressed=1;
                if(event.type==NSEventTypeLeftMouseUp) released=1;
            }
            return event;
        }] retain];
}
void hj_platform_poll(int *out) {out[0]=pressed;out[1]=released;pressed=released=0;}
void hj_platform_close(void) {if(monitor){[NSEvent removeMonitor:monitor];[monitor release];monitor=nil;}}
