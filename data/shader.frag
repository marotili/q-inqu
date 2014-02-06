#version 330
out vec4 color;
in float color_in;

flat in int instanceID;
in vec2 texCoords;
flat in int image;

uniform sampler2D Texture0;
uniform sampler2D Texture1;
uniform sampler2D Texture2;
uniform sampler2D Texture3;
uniform sampler2D Texture4;
uniform sampler2D Texture5;
uniform sampler2D Texture6;


void main()
{

	if (image == 0) {
		color = texture(Texture0, texCoords);
	} else if (image == 1) {
		color = texture(Texture1, texCoords);
	}
    // color = vec4(texCoords, 0, 1);//colors[data[instanceID]];
    // color = vec4(1, 0, 0, 1);
}