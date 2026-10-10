#version 330
in vec2 uv;
out vec4 finalColor;
uniform sampler2D texture0;
uniform vec3 skyTop;
uniform vec3 skyBottom;
uniform int mode;
void main() {
    finalColor=mode==0?vec4(mix(skyBottom,skyTop,uv.y),1.0):texture(texture0,uv);
}
