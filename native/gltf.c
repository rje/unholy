// CPU-only glTF accessors; hide upstream names from raylib's own cgltf copy.
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
static cgltf_accessor *position_accessor(cgltf_primitive *primitive) {
    for(cgltf_size i=0;i<primitive->attributes_count;i++) if(primitive->attributes[i].type==cgltf_attribute_type_position) return primitive->attributes[i].data;
    return NULL;
}
// Query count first, then copy triangle positions with each node's world pose.
// This endpoint deliberately rejects skinned meshes; animation uses bind arrays.
int uh_gltf_triangles(void *p,float *out) {
    cgltf_data *data=p;cgltf_size count=0;
    if(data->extensions_required_count) return -1;
    for(cgltf_size i=0;i<data->nodes_count;i++) {
        cgltf_node *node=&data->nodes[i];if(!node->mesh) continue;
        if(node->skin) return -1;
        float m[16];cgltf_node_transform_world(node,m);
        for(cgltf_size j=0;j<node->mesh->primitives_count;j++) {
            cgltf_primitive *primitive=&node->mesh->primitives[j];
            cgltf_accessor *position=position_accessor(primitive);
            if(primitive->type!=cgltf_primitive_type_triangles || !position || position->type!=cgltf_type_vec3 || position->component_type!=cgltf_component_type_r_32f || primitive->targets_count || (primitive->indices && primitive->indices->is_sparse)) return -1;
            cgltf_size n=primitive->indices?primitive->indices->count:position->count;
            if(n%3 || count+n>1000000) return -1;
            for(cgltf_size v=0;v<n;v++) if(out) {
                cgltf_size index=primitive->indices?cgltf_accessor_read_index(primitive->indices,v):v;
                float xyz[3];if(index>=position->count || !cgltf_accessor_read_float(position,index,xyz,3)) return -1;
                for(int axis=0;axis<3;axis++) out[(count+v)*3+axis]=m[axis]*xyz[0]+m[axis+4]*xyz[1]+m[axis+8]*xyz[2]+m[axis+12];
            }
            count+=n;
        }
    }
    return (int)count;
}
