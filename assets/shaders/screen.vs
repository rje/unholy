#version 330
in vec3 vertexPosition;
out vec2 uv;
void main() {
    uv=vertexPosition.xy*0.5+0.5;
    gl_Position=vec4(vertexPosition.xy,0.0,1.0);
}
