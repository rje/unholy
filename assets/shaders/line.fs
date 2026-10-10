#version 330
out vec4 finalColor;
uniform vec3 baseColor;
void main() { finalColor=vec4(baseColor,1.0); }
