#version 330
in vec3 vertexPosition;
layout(location=3) in mat4 instanceModel;
uniform mat4 mvp;
uniform mat4 viewProjection;
uniform int useInstances;
void main() { gl_Position=useInstances==1 ? viewProjection*instanceModel*vec4(vertexPosition,1.0) : mvp*vec4(vertexPosition,1.0); }
