#version 430

uniform mat4 view;
uniform mat4 projection;

layout(binding=0) buffer VisPoints {
	vec2 visPoints[];
};

void main()
{
	vec2 pos = vec2(visPoints[gl_VertexID].x, -visPoints[gl_VertexID].y);
    gl_Position = projection*view*vec4(pos, 0.0, 1.0);
}
