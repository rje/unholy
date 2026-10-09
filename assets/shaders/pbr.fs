#version 330
in vec3 worldPosition;
in vec3 worldNormal;
in vec2 uv;
out vec4 finalColor;
uniform sampler2D texture0;
uniform vec3 baseColor;
uniform float metalness;
uniform float roughness;
uniform float emission;
uniform vec3 viewPos;
uniform vec3 sunDirection;
uniform vec3 sunColor;
uniform float sunPower;
uniform float ambientPower;
uniform float exposure;
uniform sampler2D shadowMap;
uniform mat4 lightVP;
uniform int useShadows;
uniform vec3 fogColor;
uniform float fogDensity;
uniform int lightCount;
uniform vec3 lightPosition[8];
uniform vec3 lightColor[8];
uniform float lightPower[8];
const float PI = 3.14159265359;
vec3 fresnel(float cosTheta, vec3 f0) {
    return f0 + (1.0-f0)*pow(clamp(1.0-cosTheta,0.0,1.0),5.0);
}
float distribution(float nh, float r) {
    float a = r*r;
    float a2 = a*a;
    float d = nh*nh*(a2-1.0)+1.0;
    return a2 / max(PI*d*d, 0.000001);
}
float geometry(float nv, float r) {
    float k = (r+1.0)*(r+1.0)/8.0;
    return nv/max(nv*(1.0-k)+k,0.0001);
}
vec3 brdf(vec3 n, vec3 v, vec3 l, vec3 radiance, vec3 color, vec3 f0) {
    vec3 h=normalize(v+l);
    float nv=max(dot(n,v),0.0), nl=max(dot(n,l),0.0);
    vec3 f=fresnel(max(dot(h,v),0.0),f0);
    vec3 spec=distribution(max(dot(n,h),0.0),roughness)*geometry(nv,roughness)*geometry(nl,roughness)*f/max(4.0*nv*nl,0.001);
    vec3 diffuse=(1.0-f)*(1.0-metalness)*color/PI;
    return (diffuse+spec)*radiance*nl;
}
vec3 aces(vec3 x) {
    return clamp((x*(2.51*x+0.03))/(x*(2.43*x+0.59)+0.14),0.0,1.0);
}
float sunlight(vec3 n) {
    if(useShadows==0) return 1.0;
    vec4 light=lightVP*vec4(worldPosition,1.0);
    vec3 q=light.xyz/light.w*0.5+0.5;
    if(any(lessThan(q,vec3(0))) || any(greaterThan(q,vec3(1)))) return 1.0;
    float bias=max(0.00005*(1.0-dot(n,normalize(-sunDirection))),0.00002);
    float visibility=0.0;
    for(int x=-1;x<=1;x++) for(int y=-1;y<=1;y++)
        visibility+=q.z-bias<=texture(shadowMap,q.xy+vec2(x,y)/2048.0).r?1.0:0.0;
    return visibility/9.0;
}
void main() {
    vec3 color=pow(texture(texture0,uv).rgb*baseColor,vec3(2.2));
    vec3 n=normalize(worldNormal), v=normalize(viewPos-worldPosition);
    vec3 f0=mix(vec3(0.04),color,metalness);
    vec3 result=brdf(n,v,normalize(-sunDirection),sunColor*sunPower*sunlight(n),color,f0);
    for(int i=0;i<lightCount;i++) {
        vec3 d=lightPosition[i]-worldPosition;
        result+=brdf(n,v,normalize(d),lightColor[i]*lightPower[i]/max(dot(d,d),0.1),color,f0);
    }
    // A simple ambient approximation; this initial renderer has no environment IBL.
    result+=color*ambientPower*(0.5+0.5*max(n.y,0.0))*(1.0-metalness*0.6)+color*emission;
    vec3 mapped=pow(aces(result*exposure),vec3(1.0/2.2));
    float fog=1.0-exp(-fogDensity*length(viewPos-worldPosition));
    finalColor=vec4(mix(mapped,fogColor,fog),1.0);
}
