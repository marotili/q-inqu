#version 430

in vec2 mesh;
in float position;
in float color_in;

out float color_out;

flat out int instanceID;

uniform mat4 view;
uniform mat4 projection;

layout(binding=1) buffer Pos {
	vec2 pos[];
};

layout(binding=2) buffer Debug {
	vec4 debug[];
};

void main()
{
	instanceID = gl_InstanceID;
	color_out = color_in;
    gl_Position = projection*view*vec4(vec3(pos[instanceID], 0) + vec3(mesh, 0.0), 1.0);
    // debug[instanceID + gl_VertexID] = gl_Position;
    // debug[0] = instanceID;
    debug[instanceID*6+gl_VertexID] = gl_Position;
}
