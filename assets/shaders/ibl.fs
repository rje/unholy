#version 330
in vec2 uv;
out vec4 finalColor;
uniform sampler2D environmentMap;
uniform int mode;
uniform int face;
uniform float roughnessLevel;
const float PI=3.14159265359;
const uint SAMPLES=256u;
float radicalInverse(uint bits) {
    bits=(bits<<16u)|(bits>>16u);
    bits=((bits&0x55555555u)<<1u)|((bits&0xAAAAAAAAu)>>1u);
    bits=((bits&0x33333333u)<<2u)|((bits&0xCCCCCCCCu)>>2u);
    bits=((bits&0x0F0F0F0Fu)<<4u)|((bits&0xF0F0F0F0u)>>4u);
    bits=((bits&0x00FF00FFu)<<8u)|((bits&0xFF00FF00u)>>8u);
    return float(bits)*2.3283064365386963e-10;
}
vec2 sequence(uint i) { return vec2(float(i)/float(SAMPLES),radicalInverse(i)); }
vec3 faceDirection(vec2 p) {
    if(face==0) return normalize(vec3(1,-p.y,-p.x));
    if(face==1) return normalize(vec3(-1,-p.y,p.x));
    if(face==2) return normalize(vec3(p.x,1,p.y));
    if(face==3) return normalize(vec3(p.x,-1,-p.y));
    if(face==4) return normalize(vec3(p.x,-p.y,1));
    return normalize(vec3(-p.x,-p.y,-1));
}
vec3 radiance(vec3 d,float lod) {
    // Decoded Radiance scanlines start at the top of the image.
    vec2 st=vec2(atan(d.z,d.x)/(2.0*PI)+0.5,acos(clamp(d.y,-1.0,1.0))/PI);
    return textureLod(environmentMap,st,lod).rgb;
}
mat3 basis(vec3 n) {
    vec3 t=normalize(cross(abs(n.z)<0.999?vec3(0,0,1):vec3(1,0,0),n));
    return mat3(t,cross(n,t),n);
}
vec3 ggx(vec2 xi,float r) {
    float a=r*r, phi=2.0*PI*xi.x;
    float cosTheta=sqrt((1.0-xi.y)/(1.0+(a*a-1.0)*xi.y));
    float sinTheta=sqrt(max(0.0,1.0-cosTheta*cosTheta));
    return vec3(cos(phi)*sinTheta,sin(phi)*sinTheta,cosTheta);
}
float geometry(float nv,float r) {
    float k=r*r/2.0;
    return nv/max(nv*(1.0-k)+k,0.00001);
}
void main() {
    if(mode==2) {
        float nv=max(uv.x,0.0001), r=uv.y;
        vec3 v=vec3(sqrt(1.0-nv*nv),0,nv);
        vec2 result=vec2(0);
        for(uint i=0u;i<SAMPLES;i++) {
            vec3 h=ggx(sequence(i),r), l=normalize(2.0*dot(v,h)*h-v);
            float nl=max(l.z,0.0), nh=max(h.z,0.0), vh=max(dot(v,h),0.0);
            if(nl>0.0) {
                float visibility=geometry(nv,r)*geometry(nl,r)*vh/max(nh*nv,0.00001);
                float fc=pow(1.0-vh,5.0);
                result+=vec2((1.0-fc)*visibility,fc*visibility);
            }
        }
        finalColor=vec4(result/float(SAMPLES),0,1);return;
    }
    vec3 n=faceDirection(uv*2.0-1.0), result=vec3(0);
    mat3 frame=basis(n);
    float weight=0.0;
    for(uint i=0u;i<SAMPLES;i++) {
        vec2 xi=sequence(i);
        if(mode==0) {
            float phi=2.0*PI*xi.x, r=sqrt(xi.y);
            vec3 l=frame*vec3(r*cos(phi),r*sin(phi),sqrt(1.0-xi.y));
            // Cosine-weighted integration stores irradiance / PI.
            result+=radiance(l,0.0);weight+=1.0;
        } else {
            vec3 h=frame*ggx(xi,max(roughnessLevel,0.001));
            vec3 l=normalize(2.0*dot(n,h)*h-n);
            float nl=max(dot(n,l),0.0);
            if(nl>0.0) {
                float nh=max(dot(n,h),0.0), a=pow(max(roughnessLevel,0.001),2.0);
                float denominator=nh*nh*(a*a-1.0)+1.0;
                float pdf=max(a*a/(PI*denominator*denominator)*0.25,0.00001);
                float pixelAngle=4.0*PI/float(textureSize(environmentMap,0).x*textureSize(environmentMap,0).y);
                float lod=roughnessLevel<0.01?0.0:max(0.0,0.5*log2(1.0/(float(SAMPLES)*pdf*pixelAngle)));
                result+=radiance(l,lod)*nl;weight+=nl;
            }
        }
    }
    finalColor=vec4(result/max(weight,0.0001),1);
}
