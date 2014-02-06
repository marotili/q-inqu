#version 330

in float position;
in float color_in;

out float color_out;
 
flat out int instanceID;
out vec2 texCoords;
flat out int image;
uniform mat4 view;
uniform mat4 projection;

// uniform numTypes;
uniform int numTileSets; // needed for dynamic lookup

// Type
layout(std140) uniform LayerData {
	ivec4 tileId[2500];
};

// layout (binding=6) uniform Mesh {
// 	vec2 tileMesh[10];
// };

struct TileSet {
	int firstgid;
	int imageWidth;
	int imageHeight;
	int spacing;
	int margin;
	int tileWidth;
	int tileHeight;
	int tilesetImage;
	// int padding[8];
};

layout(std140) uniform TileSets {
	TileSet tileSets[10];
} tileSetTest;

// Coord
layout(std140) uniform Pos {
	vec4 pos[2500];
};

// layout(binding=3) buffer ObjectData {
// 	vec2 objectImage[];
// };

// layout(binding=2, std430) buffer Debug {
// 	int debug;
// }

void main()
{
	instanceID = gl_InstanceID;

	int tileGid = tileId[instanceID].x;

	// if (tileGid == 0) {
		// gl_Position = vec4(0, 0, 0, 0);
		// return;
	// }

	int myTileSet = 0; 
	for (int i = 0; i < numTileSets; i++) {
		int numX = tileSetTest.tileSets[i].imageWidth / tileSetTest.tileSets[i].tileWidth;
		int numY = tileSetTest.tileSets[i].imageHeight / tileSetTest.tileSets[i].tileHeight;

		if (tileSetTest.tileSets[i].firstgid <= tileGid && tileSetTest.tileSets[i].firstgid + numX*numY > tileGid) {
			myTileSet = i;
			break;
		}
	}

	TileSet tileSet = tileSetTest.tileSets[myTileSet];
	int localTileId = tileId[instanceID].x - tileSet.firstgid;

	int numX = tileSet.imageWidth / tileSet.tileWidth;
	int numY = tileSet.imageHeight / tileSet.tileHeight;

	int x = localTileId % numX;
	int y = localTileId / numX;

	int tx = x*tileSet.tileWidth + tileSet.spacing*(x) + tileSet.margin;
	int ty = y*tileSet.tileHeight + tileSet.spacing*(y) + tileSet.margin;

	vec2 tileMeshCoords = vec2(0, 0);
	// tx = 0;
	// ty = 0;
	if (gl_VertexID == 0) {
		tx += 0;
		ty += 0;
	} else if (gl_VertexID == 1) {
		tileMeshCoords = vec2(tileSet.tileWidth, 0);
		tx += tileSet.tileWidth;
		ty += 0;
	} else if (gl_VertexID == 2) {
		tileMeshCoords = vec2(tileSet.tileWidth, tileSet.tileHeight);
		tx += tileSet.tileWidth;
		ty += tileSet.tileHeight;
	} else if (gl_VertexID == 3) {
		tileMeshCoords = vec2(tileSet.tileWidth, tileSet.tileHeight);
		tx += tileSet.tileWidth;
		ty += tileSet.tileHeight;
	} else if (gl_VertexID == 4) {
		tileMeshCoords = vec2(0, tileSet.tileHeight);
		tx += 0;
		ty += tileSet.tileHeight;
	} else if (gl_VertexID == 5) {
		tileMeshCoords = vec2(0, 0);
		tx += 0;
		ty += 0;
	}

	texCoords = vec2(float(tx) / float(tileSet.imageWidth), 
		float(ty) / float(tileSet.imageHeight));

	vec2 newPos = pos[instanceID].xy;

	// color_out = color_in;
    gl_Position = projection*view*vec4(vec3(newPos, 0) + vec3(tileMeshCoords, 0.0), 1.0);
    // debug[1] = instanceID;
    // debug[2] = tileGid;
    // debug[instanceID*6+gl_VertexID] = gl_Position;
    texCoords = texCoords;
    image = tileSet.tilesetImage;
}
