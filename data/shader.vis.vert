#version 330

uniform mat4 view;
uniform mat4 projection;

layout(std140) uniform VisPoints {
	vec2 visPoints[100];
};

void main()
{
	vec2 pos = vec2(visPoints[gl_VertexID].x, visPoints[gl_VertexID].y);
    gl_Position = projection*view*vec4(pos, 0.0, 1.0);
}
