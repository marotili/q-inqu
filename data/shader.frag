#version 430
out vec4 color;
in float color_in;

flat in int instanceID;
in vec2 texCoords;

uniform sampler2D Tex1;

uniform vec4 colors[3] = {
	vec4(0, 1, 0, 1),
	vec4(1, 1, 0, 1),
	vec4(0, 0, 1, 1)
};

void main()
{

	color = texture(Tex1, texCoords);
    // color = vec4(texCoords, 0, 1);//colors[data[instanceID]];
    // color = colors[1];
}