#version 330

uniform mat4 view;
uniform mat4 projection;
uniform int numLights = 0;

out vec4 position;

struct Light {
	vec2 pos;
	float intensity;
	float padding;
};

layout(std140) uniform LightUBO {
	Light lights[5];
};

void main()
{
	vec2 tileMeshCoords = vec2(0,0);
	if (gl_VertexID == 0) {
		tileMeshCoords = vec2(-9000, 9000);
	} else if (gl_VertexID == 1) {
		tileMeshCoords = vec2(9000, 9000);
	} else if (gl_VertexID == 2) {
		tileMeshCoords = vec2(9000, -9000);
	} else if (gl_VertexID == 3) {
		tileMeshCoords = vec2(9000, -9000);
	} else if (gl_VertexID == 4) {
		tileMeshCoords = vec2(-9000, -9000);
	} else if (gl_VertexID == 5) {
		tileMeshCoords = vec2(-9000, 9000);
	}

    gl_Position = projection*view*vec4(vec2(tileMeshCoords), 0.0, 1.0);
    position = vec4(vec2(tileMeshCoords), 0.0, 1.0);
}
