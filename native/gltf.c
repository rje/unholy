// CPU-only glTF accessors; hide upstream names from raylib's own cgltf copy.
#include <stddef.h>
#include <stdint.h>
#include <assert.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <float.h>
#include <math.h>
#pragma GCC visibility push(hidden)
#define CGLTF_IMPLEMENTATION
#include "vendor/cgltf.h"
#pragma GCC visibility pop

void *uh_gltf_load(const char *path) {
    cgltf_options options={0};cgltf_data *data=NULL;
    if(cgltf_parse_file(&options,path,&data)!=cgltf_result_success) return NULL;
    if(cgltf_load_buffers(&options,data,path)!=cgltf_result_success || cgltf_validate(data)!=cgltf_result_success) {cgltf_free(data);return NULL;}
    return data;
}
void uh_gltf_free(void *data) {cgltf_free(data);}
// Encoded GLB/data-URI image bytes; pixel decoding stays in the image adapter.
int uh_gltf_image_bytes(void *p,int index,uint8_t *out) {
    cgltf_data *data=p;
    if(index<0 || (cgltf_size)index>=data->images_count) return 0;
    cgltf_image *image=&data->images[index];
    if(image->uri && strncmp(image->uri,"data:",5)==0) {
        const char *comma=strchr(image->uri,',');
        if(!comma || comma-image->uri<7 || strncmp(comma-7,";base64",7)!=0) return 0;
        const char *base64=comma+1;
        size_t length=strlen(base64);
        if(!length || length%4 || length>(64*1024*1024/3+1)*4) return 0;
        size_t padding=(base64[length-1]=='=')+(length>1 && base64[length-2]=='=');
        size_t size=length/4*3-padding;
        if(!size || size>64*1024*1024) return 0;
        for(size_t i=0;i<length-padding;i++) {
            char c=base64[i];
            if(!((c>='A' && c<='Z') || (c>='a' && c<='z') || (c>='0' && c<='9') || c=='+' || c=='/')) return 0;
        }
        if(out) {
            cgltf_options options={0};void *decoded=NULL;
            if(cgltf_load_buffer_base64(&options,size,base64,&decoded)!=cgltf_result_success) return 0;
            memcpy(out,decoded,size);free(decoded);
        }
        return (int)size;
    }
    if(!image->buffer_view || image->buffer_view->size>64*1024*1024) return 0;
    const uint8_t *bytes=cgltf_buffer_view_data(image->buffer_view);
    if(!bytes) return 0;
    if(out) memcpy(out,bytes,image->buffer_view->size);
    return (int)image->buffer_view->size;
}
// Copy an accessor into engine-owned floats, including normalized/sparse values.
int uh_gltf_accessor(void *p,int index,int *metadata,float *out) {
    cgltf_data *data=p;
    if(index<0 || (cgltf_size)index>=data->accessors_count) return 0;
    cgltf_accessor *a=&data->accessors[index];
    cgltf_size components=cgltf_num_components(a->type);
    if(!components || a->count>1000000/components) return 0;
    metadata[0]=(int)a->count;metadata[1]=(int)components;
    if(out && cgltf_accessor_unpack_floats(a,out,a->count*components)!=a->count*components) return 0;
    return 1;
}
