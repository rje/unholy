#version 330
in vec3 vertexPosition;
layout(location=3) in mat4 instanceModel;
uniform mat4 mvp;
uniform mat4 viewProjection;
uniform int useInstances;
// UNHOLY_SKINNING
void main() {
    vec4 position=vec4(uhSkin(vertexPosition,1.0,0),1.0);
    gl_Position=useInstances==1 ? viewProjection*instanceModel*position : mvp*position;
}
