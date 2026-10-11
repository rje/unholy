// Shared by the PBR and depth vertex passes; Haskell packs column matrices.
layout(location=11) in vec4 vertexJoints;
layout(location=12) in vec4 vertexWeights;
uniform int useSkinning;
uniform samplerBuffer jointPalette;
mat4 uhJoint(int joint, int normal) {
    int base = joint * 8 + normal * 4;
    return mat4(texelFetch(jointPalette,base), texelFetch(jointPalette,base+1),
                texelFetch(jointPalette,base+2), texelFetch(jointPalette,base+3));
}
vec3 uhSkin(vec3 value, float w, int normal) {
    if (useSkinning==0) return value;
    ivec4 joints = ivec4(vertexJoints);
    vec4 v = vec4(value,w);
    return vec3(vertexWeights.x * (uhJoint(joints.x,normal)*v)
              + vertexWeights.y * (uhJoint(joints.y,normal)*v)
              + vertexWeights.z * (uhJoint(joints.z,normal)*v)
              + vertexWeights.w * (uhJoint(joints.w,normal)*v));
}
