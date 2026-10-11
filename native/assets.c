#include "raylib.h"
#include "rlgl.h"
#include <stdlib.h>
#include <string.h>

// Asset decoding only. Haskell copies CPU arrays and owns all retained GPU data.
// raylib's model importer also creates temporary buffers; UnloadModel releases
// them as soon as Haskell has copied the decoded geometry.
void *hj_asset_model(const char *path) {
    Model *m=calloc(1,sizeof(Model));
    if(!m) return NULL;
    if(FileExists(path)) *m=LoadModel(path);
    if(!IsModelValid(*m)) { if(m->meshCount) UnloadModel(*m);free(m);return NULL; }
    return m;
}
void hj_asset_model_free(void *p) { if(p) {UnloadModel(*(Model *)p);free(p);} }
int hj_asset_mesh_count(void *p) {return ((Model *)p)->meshCount;}
int hj_asset_mesh(void *p,int i,int *counts,void **arrays) {
    Model *model=p;
    if(i<0||i>=model->meshCount) return 0;
    Mesh *m=&model->meshes[i];
    counts[0]=m->vertexCount;counts[1]=m->triangleCount*3;
    arrays[0]=m->vertices;arrays[1]=m->texcoords;arrays[2]=m->normals;arrays[3]=m->indices;
    return m->vertices!=NULL;
}
// Flatten the import transform in column-major order; avoid exposing C layouts.
void hj_asset_transform(void *p,float *out) {
    Matrix m=((Model *)p)->transform;
    float data[]={m.m0,m.m1,m.m2,m.m3,m.m4,m.m5,m.m6,m.m7,m.m8,m.m9,m.m10,m.m11,m.m12,m.m13,m.m14,m.m15};
    memcpy(out,data,sizeof(data));
}
void *hj_asset_image(const char *path,int *size) {
    Image *image=calloc(1,sizeof(Image));
    if(!image) return NULL;
    *image=LoadImage(path);
    if(!IsImageValid(*image)) {free(image);return NULL;}
    ImageFormat(image,PIXELFORMAT_UNCOMPRESSED_R8G8B8A8);
    size[0]=image->width;size[1]=image->height;
    return image;
}
void *hj_asset_pixels(void *p) {return ((Image *)p)->data;}
void hj_asset_image_free(void *p) {if(p) {UnloadImage(*(Image *)p);free(p);} }
void *hj_asset_mesh_image(void *p,int index,int *size) {
    Model *m=p;
    Texture2D t=m->materials[m->meshMaterial[index]].maps[MATERIAL_MAP_ALBEDO].texture;
    if(!t.id||t.id==rlGetTextureIdDefault()) return NULL;
    Image *image=calloc(1,sizeof(Image));
    if(!image) return NULL;
    *image=LoadImageFromTexture(t);
    if(!IsImageValid(*image)) {free(image);return NULL;}
    ImageFormat(image,PIXELFORMAT_UNCOMPRESSED_R8G8B8A8);
    size[0]=image->width;size[1]=image->height;
    return image;
}
