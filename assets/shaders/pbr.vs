#version 330
in vec3 vertexPosition;
in vec3 vertexNormal;
in vec2 vertexTexCoord;
layout(location=3) in mat4 instanceModel;
layout(location=7) in mat4 instanceNormal;
uniform mat4 mvp;
uniform mat4 matModel;
uniform mat4 matNormal;
uniform mat4 viewProjection;
uniform int useInstances;
out vec3 worldPosition;
out vec3 worldNormal;
out vec2 uv;
// UNHOLY_SKINNING
void main() {
    vec3 position = uhSkin(vertexPosition,1.0,0);
    vec3 direction = normalize(uhSkin(vertexNormal,0.0,1));
    mat4 model = useInstances==1 ? instanceModel : matModel;
    mat4 normal = useInstances==1 ? instanceNormal : matNormal;
    worldPosition = vec3(model * vec4(position, 1.0));
    worldNormal = normalize(vec3(normal * vec4(direction, 0.0)));
    uv = vertexTexCoord;
    gl_Position = useInstances==1 ? viewProjection * vec4(worldPosition,1.0) : mvp * vec4(position,1.0);
}
