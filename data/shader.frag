#version 430
out vec4 color;
in float color_in;

layout(binding=0) buffer Data {
	int data[];
};

flat in int instanceID;

uniform vec4 colors[3] = {
	vec4(0, 1, 0, 1),
	vec4(1, 1, 0, 1),
	vec4(0, 0, 1, 1)
};

void main()
{
    color = vec4(1, 0, 0, 1);//colors[data[instanceID]];
    // color = colors[1];
}