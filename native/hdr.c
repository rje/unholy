// Reuse stb's HDR decoder independently of raylib build-time format switches.
#define STB_IMAGE_STATIC
#define STBI_ONLY_HDR
#define STB_IMAGE_IMPLEMENTATION
#include "vendor/stb_image.h"
float *hj_hdr_load(const char *path,int *size) {
    int channels;
    return stbi_loadf(path,&size[0],&size[1],&channels,4);
}
void hj_hdr_free(void *pixels) {stbi_image_free(pixels);}
