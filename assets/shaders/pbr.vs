#version 330
in vec3 vertexPosition;
in vec3 vertexNormal;
in vec2 vertexTexCoord;
uniform mat4 mvp;
uniform mat4 matModel;
uniform mat4 matNormal;
out vec3 worldPosition;
out vec3 worldNormal;
out vec2 uv;
void main() {
    worldPosition = vec3(matModel * vec4(vertexPosition, 1.0));
    worldNormal = normalize(vec3(matNormal * vec4(vertexNormal, 0.0)));
    uv = vertexTexCoord;
    gl_Position = mvp * vec4(vertexPosition, 1.0);
}
