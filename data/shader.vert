#version 430

in vec2 mesh;
in float position;
in float color_in;

out float color_out;

flat out int instanceID;
out vec2 texCoords;
flat out int image;
uniform mat4 view;
uniform mat4 projection;

// Type
layout(binding=0) buffer LayerData {
	int tileId[];
};

struct TileSet {
	int firstgid;
	int imageWidth;
	int imageHeight;
	int spacing;
	int margin;
	int tileWidth;
	int tileHeight;
	int tilesetImage;
};

layout(binding=4) buffer TileSets {
	TileSet tileSets[];
};

uniform int renderType = 0;

// Coord
layout(binding=1) buffer Pos {
	vec2 pos[];
};

layout(binding=3) buffer ObjectData {
	vec2 objectImage[];
};

layout(binding=2) buffer Debug {
	int debug[];
};

void main()
{
	instanceID = gl_InstanceID;

	int tileGid = tileId[instanceID];

	if (tileGid == 0) {
		gl_Position = vec4(0, 0, 0, 0);
		return;
	}

	int myTileSet = 0; 
	for (int i = 0; i < tileSets.length(); i++) {
		int numX = tileSets[i].imageWidth / tileSets[i].tileWidth;
		int numY = tileSets[i].imageHeight / tileSets[i].tileHeight;

		if (tileSets[i].firstgid <= tileGid && tileSets[i].firstgid + numX*numY >= tileGid) {
			myTileSet = i;
			break;
		}
	}

	struct TileSet tileSet = tileSets[myTileSet];
	int localTileId = tileId[instanceID] - tileSet.firstgid;

	int numX = tileSet.imageWidth / tileSet.tileWidth;
	int numY = tileSet.imageHeight / tileSet.tileHeight;

	int x = localTileId % numX;
	int y = localTileId / numX;

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
    debug[instanceID] = tileSets.length();
    // debug[instanceID] = tileSets[0].imageHeight;
    // debug[1] = instanceID;
    // debug[2] = tileGid;
    // debug[instanceID*6+gl_VertexID] = gl_Position;
    texCoords = texCoords;
    image = tileSet.tilesetImage;
}
