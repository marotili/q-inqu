#version 430

in vec2 mesh;
in float position;
in float color_in;

out float color_out;

flat out int instanceID;
out vec2 texCoords;
uniform mat4 view;
uniform mat4 projection;

layout(binding=0) buffer Data {
	int tileId[];
};

uniform struct TileSet {
	int firstgid;
	int imageWidth;
	int imageHeight;
	int spacing;
	int margin;
	int tileWidth;
	int tileHeight;
} tileSets[1] = {
	struct TileSet(
		1, 192, 217, 0, 0, 24, 24
	)
};

layout(binding=1) buffer Pos {
	vec2 pos[];
};

layout(binding=2) buffer Debug {
	vec4 debug[];
};

void main()
{
	instanceID = gl_InstanceID;
	struct TileSet tileSet = tileSets[0];
	int tileId = tileId[instanceID] - tileSet.firstgid;

	int numX = tileSet.imageWidth / tileSet.tileWidth;

	int x = tileId % numX;
	int y = tileId / numX;

	int tx = x*tileSet.tileWidth + tileSet.spacing*(x) + tileSet.margin;
	int ty = y*tileSet.tileHeight + tileSet.spacing*(y) + tileSet.margin;

	// tx = 0;
	// ty = 0;
	if (gl_VertexID == 0) {
		tx += 0;
		ty += 0;
	} else if (gl_VertexID == 1) {
		tx += tileSet.tileWidth;
		ty += 0;
	} else if (gl_VertexID == 2) {
		tx += tileSet.tileWidth;
		ty += tileSet.tileHeight;
	} else if (gl_VertexID == 3) {
		tx += tileSet.tileWidth;
		ty += tileSet.tileHeight;
	} else if (gl_VertexID == 4) {
		tx += 0;
		ty += tileSet.tileHeight;
	} else if (gl_VertexID == 5) {
		tx += 0;
		ty += 0;
	}

	texCoords = vec2(float(tx) / float(tileSet.imageWidth), 
		float(ty) / float(tileSet.imageHeight));

	// color_out = color_in;
    gl_Position = projection*view*vec4(vec3(pos[instanceID], 0) + vec3(mesh, 0.0), 1.0);
    // debug[instanceID + gl_VertexID] = gl_Position;
    // debug[0] = instanceID;
    debug[instanceID*6+gl_VertexID] = gl_Position;
    texCoords = texCoords;
}
