#version 330

out vec4 color_pass;
out vec2 texCoord;
uniform mat4 view;
uniform mat4 projection;

in vec2 pos;
in vec4 myColor;
in int charId;
uniform int size = 4*256;

struct Char {
    vec2 textureCoord;
    vec2 charSize;
};

layout(std140) uniform CharMap {
    Char chars[52]; // lowercase and uppercase (english only)
};

// TODO: implement setting size
void main()
{

    color_pass = myColor;

    Char thisChar = chars[charId];

    vec2 tileMeshCoords = vec2(0,0);
    texCoord = thisChar.textureCoord;
    if (gl_VertexID % 6 == 0) {
        tileMeshCoords = size*vec2(0, 0);
        texCoord += vec2(0, thisChar.charSize.y);
    } else if (gl_VertexID % 6 == 1) {
        tileMeshCoords = size*vec2(0, thisChar.charSize.y);
        texCoord += vec2(0, 0);
    } else if (gl_VertexID % 6 == 2) {
        tileMeshCoords = size*vec2(thisChar.charSize.x, thisChar.charSize.y);
        texCoord += vec2(thisChar.charSize.x, 0);
    } else if (gl_VertexID % 6 == 3) {
        tileMeshCoords = size*vec2(thisChar.charSize.x, thisChar.charSize.y);
        texCoord += vec2(thisChar.charSize.x, 0);
    } else if (gl_VertexID % 6 == 4) {
        tileMeshCoords = size*vec2(thisChar.charSize.x, 0);
        texCoord += vec2(thisChar.charSize.x, thisChar.charSize.y);
    } else if (gl_VertexID % 6 == 5) {
        tileMeshCoords = size*vec2(0, 0);
        texCoord += vec2(0, thisChar.charSize.y);
    }

    gl_Position = projection*view*(vec4(pos, 0, 1) + 
        vec4(vec3(tileMeshCoords, 0.0), 1.0));
}
