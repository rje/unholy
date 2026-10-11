#version 330
in vec2 uv;
out vec4 finalColor;
uniform sampler2D texture0;
uniform sampler2D environmentMap;
uniform vec3 skyTop;
uniform vec3 skyBottom;
uniform int mode;
uniform int iblEnabled;
uniform float environmentIntensity;
uniform float environmentRotation;
uniform float exposure;
uniform mat4 inverseVP;
uniform vec3 viewPos;
vec3 aces(vec3 x) {
    return clamp((x*(2.51*x+0.03))/(x*(2.43*x+0.59)+0.14),0.0,1.0);
}
void main() {
    if(mode==1) { finalColor=texture(texture0,uv);return; }
    if(iblEnabled==0) { finalColor=vec4(mix(skyBottom,skyTop,uv.y),1.0);return; }
    vec4 farPoint=inverseVP*vec4(uv*2.0-1.0,1.0,1.0);
    vec3 d=normalize(farPoint.xyz/farPoint.w-viewPos);
    float c=cos(environmentRotation),s=sin(environmentRotation);
    d=vec3(c*d.x+s*d.z,d.y,-s*d.x+c*d.z);
    vec2 st=vec2(atan(d.z,d.x)/6.28318530718+0.5,acos(clamp(d.y,-1.0,1.0))/3.14159265359);
    finalColor=vec4(pow(aces(textureLod(environmentMap,st,0.0).rgb*environmentIntensity*exposure),vec3(1.0/2.2)),1.0);
}
