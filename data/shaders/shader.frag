#version 330
out vec4 color;

in vec2 texCoords;
flat in int image;

uniform sampler2D Texture0;
uniform sampler2D Texture1;
uniform sampler2D Texture2;
uniform sampler2D Texture3;
uniform sampler2D Texture4;
uniform sampler2D Texture5;
uniform sampler2D Texture6;
uniform sampler2D Texture7;
uniform sampler2D Texture8;
uniform sampler2D Texture9;
uniform sampler2D Texture10;
uniform sampler2D Texture11;
uniform sampler2D Texture12;

uniform int ghostMode = 0;


void main()
{
	switch (image) {
		case 0:
			color = texture(Texture0, texCoords);
			break;
		case 1:
			color = texture(Texture1, texCoords);
			break;
		case 2:
			color = texture(Texture2, texCoords);
			break;
		case 3:
			color = texture(Texture3, texCoords);
			break;
		case 4:
			color = texture(Texture4, texCoords);
			break;
		case 5:
			color = texture(Texture5, texCoords);
			break;
		case 6:
			color = texture(Texture6, texCoords);
			break;
		case 7:
			color = texture(Texture7, texCoords);
			break;
		case 8:
			color = texture(Texture8, texCoords);
			break;
		case 9:
			color = texture(Texture9, texCoords);
			break;
		case 10:
			color = texture(Texture10, texCoords);
			break;
		case 11:
			color = texture(Texture11, texCoords);
			break;
		case 12:
			color = texture(Texture12, texCoords);
			break;
		default:
			color = vec4(0, 0, 0, 1);
	}

	if (ghostMode == 2) {
	vec4 grayscale = vec4(3, 0.59, 0.11, 1);
	float gray = 0.3*color.r + 0.59*color.g + 0.11*color.b;
	color = vec4(max(gray, color.r), gray, gray, color.a);
	}
	// color = vec4(1, 0, 0, 1);
     //color = vec4(texCoords, 0, 1);//colors[data[instanceID]];
    // color = vec4(1, 0, 0, 1);
}
