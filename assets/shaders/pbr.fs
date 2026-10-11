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
uniform int baseColorLinear;
uniform sampler2D normalMap;
uniform sampler2D metalMap;
uniform sampler2D roughMap;
uniform sampler2D emissiveMap;
uniform sampler2D occlusionMap;
uniform vec3 emissiveColor;
uniform int useNormalMap;
uniform float normalScale;
uniform float occlusionStrength;
uniform vec3 viewPos;
uniform int iblEnabled;
uniform float environmentIntensity;
uniform float environmentRotation;
uniform float prefilterLevels;
uniform samplerCube irradianceMap;
uniform samplerCube prefilterMap;
uniform sampler2D brdfLUT;
vec3 environmentDirection(vec3 d) {
    float c=cos(environmentRotation),s=sin(environmentRotation);
    return vec3(c*d.x+s*d.z,d.y,-s*d.x+c*d.z);
}
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
float surfaceRoughness;
float surfaceMetalness;
vec3 srgbToLinear(vec3 c) {
    return mix(c/12.92,pow((c+0.055)/1.055,vec3(2.4)),step(vec3(0.04045),c));
}
vec3 mappedNormal(vec3 n) {
    if(useNormalMap==0) return n;
    vec3 dx=dFdx(worldPosition), dy=dFdy(worldPosition);
    vec2 tx=dFdx(uv), ty=dFdy(uv);
    float determinant=tx.x*ty.y-tx.y*ty.x;
    if(abs(determinant)<0.00000001) return n;
    vec3 tangent=(dx*ty.y-dy*tx.y)/determinant;
    vec3 bitangent=(-dx*ty.x+dy*tx.x)/determinant;
    tangent-=n*dot(n,tangent);
    if(dot(tangent,tangent)<0.00000001) return n;
    tangent=normalize(tangent);
    float handedness=dot(cross(n,tangent),bitangent)<0.0?-1.0:1.0;
    bitangent=normalize(cross(n,tangent))*handedness;
    vec3 sampled=texture(normalMap,uv).xyz*2.0-1.0;
    sampled.xy*=normalScale;
    vec3 result=mat3(tangent,bitangent,n)*sampled;
    return dot(result,result)>0.00000001?normalize(result):n;
}
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
    vec3 spec=distribution(max(dot(n,h),0.0),surfaceRoughness)*geometry(nv,surfaceRoughness)*geometry(nl,surfaceRoughness)*f/max(4.0*nv*nl,0.001);
    vec3 diffuse=(1.0-f)*(1.0-surfaceMetalness)*color/PI;
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
    float bias=max(0.00035*(1.0-dot(n,normalize(-sunDirection))),0.00010);
    float visibility=0.0;
    for(int x=-1;x<=1;x++) for(int y=-1;y<=1;y++)
        visibility+=q.z-bias<=texture(shadowMap,q.xy+vec2(x,y)/2048.0).r?1.0:0.0;
    return visibility/9.0;
}
void main() {
    vec3 color=texture(texture0,uv).rgb*(baseColorLinear==1?baseColor:srgbToLinear(baseColor));
    surfaceMetalness=clamp(metalness*texture(metalMap,uv).r,0.0,1.0);
    surfaceRoughness=clamp(roughness*texture(roughMap,uv).r,0.04,1.0);
    vec3 n=mappedNormal(normalize(worldNormal)), v=normalize(viewPos-worldPosition);
    vec3 f0=mix(vec3(0.04),color,surfaceMetalness);
    vec3 result=brdf(n,v,normalize(-sunDirection),sunColor*sunPower*sunlight(n),color,f0);
    for(int i=0;i<lightCount;i++) {
        vec3 d=lightPosition[i]-worldPosition;
        result+=brdf(n,v,normalize(d),lightColor[i]*lightPower[i]/max(dot(d,d),0.1),color,f0);
    }

    float occlusion=mix(1.0,texture(occlusionMap,uv).r,occlusionStrength);
    if(iblEnabled==1) {
        float nv=max(dot(n,v),0.0);
        vec3 f=f0+(max(vec3(1.0-surfaceRoughness),f0)-f0)*pow(1.0-nv,5.0);
        vec3 diffuse=(1.0-f)*(1.0-surfaceMetalness)*color*texture(irradianceMap,environmentDirection(n)).rgb;
        vec3 reflected=textureLod(prefilterMap,environmentDirection(reflect(-v,n)),surfaceRoughness*prefilterLevels).rgb;
        vec2 integrated=texture(brdfLUT,vec2(nv,surfaceRoughness)).rg;
        result+=(diffuse+reflected*(f0*integrated.x+integrated.y))*environmentIntensity*occlusion;
    } else result+=color*ambientPower*(0.5+0.5*max(n.y,0.0))*(1.0-surfaceMetalness*0.6)*occlusion;
    result+=color*emission+texture(emissiveMap,uv).rgb*emissiveColor;
    vec3 mapped=pow(aces(result*exposure),vec3(1.0/2.2));
    float fog=1.0-exp(-fogDensity*length(viewPos-worldPosition));
    finalColor=vec4(mix(mapped,fogColor,fog),1.0);
}
